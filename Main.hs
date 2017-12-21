{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception (SomeException)
import           Control.Exception.Lifted (handle)
import           Control.Monad (unless, when, replicateM_)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State (StateT, execStateT, get, modify)
import           Data.Bits (testBit)
import qualified Data.Foldable as F
import           Data.List (dropWhileEnd)
import qualified Data.Map as Map
import           Data.Maybe (Maybe(..), isJust, fromJust, fromMaybe, maybe)
import           Data.Sequence ((|>))
import qualified Data.Sequence as S
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Graphics.Vty ((<|>), (<->))
import qualified Graphics.Vty as Vty
import           Prelude hiding (or)
import qualified System.Directory as Directory
import           System.FilePath (FilePath, (</>), takeDirectory, takeExtension, takeFileName)
import           System.IO.Unsafe (unsafePerformIO)
import qualified System.Posix.Directory as Posix
import           System.Posix.Files (FileStatus, fileOwner, fileGroup, fileMode, fileSize,
                                     getSymbolicLinkStatus, isDirectory, isSymbolicLink)
import           System.Posix.StatVFS

data Entry = Entry { entryPath :: FilePath, entryStatus :: FileStatus }

instance Eq Entry where
    e1 == e2 = entryPath e1 == entryPath e2

-- Make sure that ".." is always ordered first.
instance Ord Entry where
    e1 `compare` e2 | takeFileName p1 == ".." = LT
                    | takeFileName p2 == ".." = GT
                    | otherwise = p1 `compare` p2
      where p1 = entryPath e1
            p2 = entryPath e2


data Pane =
    Pane { paneEntries   :: S.Seq Entry
         , paneEntryIdx  :: Int
         , paneDirectory :: FilePath
         , paneStatVFS   :: StatVFS
         , paneLastPath  :: FilePath
         , paneIsActive  :: Bool
         , paneWidth     :: Int
         , paneHeight    :: Int
         , paneTop       :: Int
         }

loadEntries :: FilePath -> IO (S.Seq Entry)
loadEntries path = Directory.withCurrentDirectory path $ Posix.openDirStream path >>= go S.empty
    where
      go result ds = do
          file <- Posix.readDirStream ds
          case file of
            ""  -> return result
            "." -> go result ds
            f   -> if path == "/" && f == ".."
                   then go result ds
                   else do
                       stat <- getSymbolicLinkStatus f
                       go (result |> Entry f stat) ds

loadPane :: FilePath -> IO Pane
loadPane path = do
    es <- loadEntries path
    vfs <- statVFS path
    return Pane { paneEntries = S.sort es
                , paneEntryIdx = 0
                , paneDirectory = path
                , paneStatVFS = vfs
                , paneLastPath = takeFileName path
                , paneIsActive = True
                , paneWidth = getPaneWidth
                , paneHeight = getPaneHeight
                , paneTop = 0
                }

firstEntry :: Pane -> Pane
firstEntry p = p { paneEntryIdx = 0 }

clampEntryIdx :: Pane -> Pane
clampEntryIdx p = p { paneEntryIdx = max 0 $ min (S.length (paneEntries p) - 1) (paneEntryIdx p) }

nextEntry :: Pane -> Pane
nextEntry p = clampEntryIdx $ p { paneEntryIdx = paneEntryIdx p + 1 }

prevEntry :: Pane -> Pane
prevEntry p = clampEntryIdx $ p { paneEntryIdx = paneEntryIdx p - 1 }

adjustView :: Pane -> Pane
adjustView p = p { paneTop = top }
    where top | current >= height + t - 1 = current - height + 1
              | current <  t = current
              | otherwise = t
          current = paneEntryIdx p
          height  = paneHeight p
          t = paneTop p

paneEntry :: Pane -> Entry
paneEntry p = S.index (paneEntries p) (paneEntryIdx p)


data ColumnType = Name
                | User
                | Group
                | Permissions
                | Size
                deriving Eq

type Column = (ColumnType, Int) -- (type, width)

data DisplayRules =
    DisplayRules { drDirectory :: Vty.Attr
                 , drSymlink   :: Vty.Attr
                 , drFiles     :: Map.Map Text Vty.Attr -- file suffix -> attribute
                 }

data Dialog = SimpleDialog { dPrompt :: Text
                           , dSource :: Text
                           , dTarget :: Text
                           , dFinish :: App ()
                           , dCancel :: App ()
                           }
--            | YesNoDialog { ... }

insertChar :: Char -> Dialog -> Dialog
insertChar c dlg = dlg { dTarget = dTarget dlg `T.append` T.singleton c }

backspace :: Dialog -> Dialog
backspace dlg = dlg { dTarget = T.dropEnd 1 $ dTarget dlg }

clear :: Dialog -> Dialog
clear dlg = dlg { dTarget = T.empty }

data AppState =
    AppState { asActivePane :: Pane
             , asOtherPane :: Pane
             , asActiveLeft :: Bool
             , asDialog :: Maybe Dialog
             , asError :: Maybe Text
             , asHelp :: Vty.Image
             , asQuit :: Bool
             }

type App = StateT AppState IO


{-# NOINLINE terminalBounds #-}
terminalBounds :: Vty.DisplayRegion
terminalBounds = unsafePerformIO $ Vty.outputForConfig Vty.defaultConfig >>= Vty.displayBounds

terminalWidth :: Int
terminalWidth = fst terminalBounds

terminalHeight :: Int
terminalHeight = snd terminalBounds

getPaneWidth :: Int
getPaneWidth = terminalWidth `div` 2 - 1

getPaneHeight :: Int
getPaneHeight = terminalHeight - 5


errorDialog :: Text -> [Vty.Image]
errorDialog msg = [ Vty.translate (x+1) (y+1) $ drawErrorDialog msg
                  , Vty.translate x y $ drawFrame (l + 2) 3
                  ]
                  where l = fromIntegral $ T.length msg
                        x = (terminalWidth - l) `div` 2
                        y = 0

drawErrorDialog :: Text -> Vty.Image
drawErrorDialog = Vty.text (Vty.defAttr `Vty.withForeColor` Vty.red)

drawFrame :: Int -> Int -> Vty.Image
drawFrame width height =
    char '┏' <|> horiz <|> char '┓'
    <->
    foldr (<->)
          Vty.emptyImage
          (replicate (height - 2) (char '┃' <|> Vty.translateX wfill (char '┃')))
    <->
    char '┗' <|> horiz <|> char '┛'
    where char = Vty.char Vty.defAttr
          wfill = width - 2
          horiz = Vty.charFill Vty.defAttr '━' wfill 1

humanReadable :: Integer -> Text
humanReadable size
    | size > gb = T.pack $ show (size `div` gb)   ++ "G"
    | size > mb = T.pack $ show (size `div` mb)   ++ "M"
    | size > k  = T.pack $ show (size `div` 1024) ++ "K"
    | otherwise = T.pack $ show size
    where k  =   10 ^ (5 :: Integer)
          mb = 1024 ^ (2 :: Integer)
          gb = 1024 ^ (3 :: Integer)

drawColumn :: DisplayRules -> Entry -> Bool -> Bool -> Column -> Vty.Image
drawColumn rules entry active selected column =
    if ctype == Name
    then img <|> fill
    else fill <|> img
    where (ctype, width) = column
          img  = case ctype of
                   Name -> nameImg
                   Permissions -> permissionsImg
                   Size -> sizeImg
                   User -> userImg
                   Group -> groupImg
          fill = Vty.charFill attr ' ' (width - Vty.imageWidth img) 1
          path = entryPath entry
          stat = entryStatus entry
          ext = T.pack $ takeExtension path
          attr' | isDirectory stat    = drDirectory rules
                | isSymbolicLink stat = drSymlink rules
                | otherwise = fromMaybe Vty.defAttr $ Map.lookup ext $ drFiles rules
          attr = if selected
                 then attr' `Vty.withForeColor` Vty.black `Vty.withBackColor` if active
                                                                             then Vty.green
                                                                             else Vty.white
                 else attr'

          nameImg = Vty.string attr $ take width path

          permissionsImg = Vty.string attr modes
          mode  = fileMode $ entryStatus entry
          modes = let ur = if testBit mode 8 then 'r' else '-'
                      uw = if testBit mode 7 then 'w' else '-'
                      ux = if testBit mode 6 then 'x' else '-'
                      gr = if testBit mode 5 then 'r' else '-'
                      gw = if testBit mode 4 then 'w' else '-'
                      gx = if testBit mode 3 then 'x' else '-'
                      or = if testBit mode 2 then 'r' else '-'
                      ow = if testBit mode 1 then 'w' else '-'
                      ox = if testBit mode 0 then 'x' else '-'
                  in [ur, uw, ux, gr, gw, gx, or, ow, ox]

          sizeImg = Vty.text attr $ humanReadable $ fromIntegral $ fileSize stat

          -- TODO: Show real names (System.Posix.User).
          userImg = Vty.string attr $ show $ fileOwner stat

          groupImg = Vty.string attr $ show $ fileGroup stat


drawColumns :: DisplayRules -> Entry -> Bool -> Bool -> [Column] -> Vty.Image
drawColumns r e a s = foldr ((<|>) . drawColumn r e a s) Vty.emptyImage

display :: Vty.Vty -> App ()
display vty = do
    s <- get
    let eDialog = case asError s of
                    Nothing -> []
                    Just e -> errorDialog e
        dialog = case asDialog s of
                   Nothing -> Vty.emptyImage
                   Just d -> foldr (\t img -> Vty.text Vty.defAttr t <|> img)
                                   Vty.emptyImage
                                   [dPrompt d, dSource d, " → ", dTarget d]
        hx = (terminalWidth - helpWidth) `div` 2
        help  = if asHelp s == Vty.emptyImage
                then [Vty.emptyImage]
                else fmap (Vty.translateX hx) [drawFrame (helpWidth + 2) (helpHeight + 2) , Vty.translate 1 1 $ asHelp s]
        pane  = asActivePane s
        other = asOtherPane s
        pw = paneWidth pane
        ph = paneHeight pane
        rx = pw + 1
        -- TODO: This doesn't feel right. On the application state level, I know what pane is active,
        --       so the paneIsActive shouldn't be needed at all. However, I need it.
        --       Figure this out.
        tleft  = if asActiveLeft s then 1 else rx + 1
        tright = if asActiveLeft s then rx + 1 else 1
        layers = help ++ eDialog ++ [
                                    -- Left pane.
                                      Vty.translateX tleft (drawPane (paneIsActive pane) pane)
                                    -- Right pane.
                                    , Vty.translateX tright (drawPane (paneIsActive other) other)
                                    -- Dialog.
                                    , Vty.translateY (ph + 2) dialog
                                    -- Left frame.
                                    , drawFrame (pw + 1) (ph + 2)
                                    -- Right frame.
                                    , Vty.translateX rx (drawFrame (pw + 1) (ph + 2))
                                    ]
    lift $ Vty.update vty $ Vty.picForLayers layers
    where
      drawPane :: Bool -> Pane -> Vty.Image
      drawPane active pane = header <-> drawEntries before <-> drawEntry True (S.index focus 0) <-> drawEntries after
        where
          items = S.take (paneHeight pane) $ S.drop (paneTop pane) $ paneEntries pane
          idx = paneEntryIdx pane - paneTop pane
          (before, rest) = S.splitAt idx items
          (focus, after) = S.splitAt 1 rest
          columns = [(User, 6), (Group, 6), (Permissions, 10), (Size, 7)]
          columnsWidth = sum $ fmap snd columns
          rules = DisplayRules { drDirectory = Vty.defAttr `Vty.withForeColor` Vty.blue
                               , drSymlink   = Vty.defAttr `Vty.withForeColor` Vty.magenta
                               , drFiles = Map.fromList [(".hs", Vty.defAttr `Vty.withForeColor` Vty.red)
                                                        ,(".cabal", Vty.defAttr `Vty.withForeColor` Vty.green)
                                                        ,(".md", Vty.defAttr `Vty.withForeColor` Vty.yellow)
                                                        ]
                               }

          header = Vty.string (Vty.defAttr `Vty.withStyle` Vty.bold) $ take (paneWidth pane - 1) $ paneDirectory pane

          drawEntry :: Bool -> Entry -> Vty.Image
          drawEntry selected e = drawColumns rules e active selected $ (Name, paneWidth pane - columnsWidth - 1) : columns

          drawEntries :: S.Seq Entry -> Vty.Image
          drawEntries = F.foldr (\e img -> drawEntry False e <-> img) Vty.emptyImage


onError :: Text -> SomeException -> App ()
onError msg ex = modify (\s -> s { asDialog = Nothing
                                 , asError = Just $ T.concat [msg, ":", reason]
                                 })
    where reason = T.takeWhileEnd (':' /=) $ T.pack $ show ex


appFirstEntry :: App ()
appFirstEntry = modify (\s -> s { asActivePane = adjustView $ firstEntry $ asActivePane s })

appNextEntry :: App ()
appNextEntry = modify (\s -> s { asActivePane = adjustView $ nextEntry $ asActivePane s })

appPrevEntry :: App ()
appPrevEntry = modify (\s -> s { asActivePane = adjustView $ prevEntry $ asActivePane s })

appEnterEntry :: App ()
appEnterEntry = do
    s <- get
    let pane = asActivePane s
        entry = paneEntry pane
        path = entryPath entry
        next = pathUp $ paneDirectory pane </> path
    handle (onError $ T.concat ["Cannot change directory: ", T.pack next]) $ do
        let old = if path == ".."
                  then paneLastPath pane
                  else ""
        lift $ Directory.setCurrentDirectory next
        newpane <- lift $ loadPane next
        let idx = fromMaybe 0 $ S.findIndexL ((== old) . entryPath) $ paneEntries newpane
            top = max 0 (idx - paneHeight pane `div` 2)
        modify (\st -> st { asActivePane = newpane { paneEntryIdx = idx
                                                 , paneTop = top
                                                 , paneLastPath = takeFileName next
                                                 }
                          })

    where pathUp :: FilePath -> FilePath
          pathUp p | takeFileName p == ".." = takeDirectory $ takeDirectory p
                   | otherwise = p

-- TODO: This always unconditionally reloads both panes, which is usually not necessary.
--       It should only load the affected pane(s).
reloadPanes :: App ()
reloadPanes = do
    s <- get
    let apane = asActivePane s
        opane = asOtherPane s
    aentries <- lift $ loadEntries $ paneDirectory apane
    oentries <- lift $ loadEntries $ paneDirectory opane
    modify (\st -> st { asActivePane = clampEntryIdx $ apane { paneEntries = S.sort aentries }
                      , asOtherPane  = clampEntryIdx $ opane { paneEntries = S.sort oentries }
                      })

withDialog :: Text -> Text -> Text -> (Text -> Text -> App ()) -> App ()
withDialog prompt source target finish = modify (\s -> s { asDialog = Just dialog })
    where dialog = SimpleDialog { dPrompt = prompt
                                , dSource = source
                                , dTarget = target
                                , dFinish = finish'
                                , dCancel = modify (\s -> s { asDialog = Nothing })
                                }
          finish' = do
              s <- get
              maybe (return ())
                    (\d -> finish (dSource d) (dTarget d))
                    (asDialog s)


-- TODO: Refactor the dialog functions even more, especially the finish function.
--       Also, make appChangeDirectory not a special case so that the finish function can be refactored easily.
--       That means, sort out the paneTrail "problem".
appChangeDirectory :: App ()
appChangeDirectory = withDialog "cd" T.empty T.empty finish
    where finish _ dir = handle (onError $ T.concat ["Cannot change directory: ", dir]) $ do
              let path = T.unpack dir
              lift $ Directory.setCurrentDirectory path
              newpane <- lift $ loadPane path
              modify (\s -> s { asActivePane = newpane { paneLastPath = takeFileName $ dropWhileEnd (== '/') path } })
              modify (\s -> s { asDialog = Nothing })

appCopyEntry :: App ()
appCopyEntry = do
    s <- get
    let path = entryPath $ paneEntry $ asActivePane s
    withDialog "copy " (T.pack path) (T.pack $ paneDirectory $ asOtherPane s) finish
    where
      finish source target = handle (onError $ T.concat ["Cannot copy: ", source, " → ", target]) $ do
              when (not $ source == target || target == T.empty) $ do
                  lift $ Directory.copyFileWithMetadata (T.unpack source) (T.unpack target)
                  reloadPanes
              modify (\s -> s { asDialog = Nothing })

appMoveEntry :: App ()
appMoveEntry = do
    s <- get
    let path = entryPath $ paneEntry $ asActivePane s
    withDialog "move " (T.pack path) T.empty finish
    where
      finish source target = handle (onError $ T.concat ["Cannot move: ", source, " → ", target]) $ do
              when (not $ source == target || target == T.empty) $ do
                  lift $ Directory.renamePath (T.unpack source) (T.unpack target)
                  reloadPanes
              modify (\s -> s { asDialog = Nothing })

appDeleteEntry :: App ()
appDeleteEntry = do
    s <- get
    let path = entryPath $ paneEntry $ asActivePane s
    withDialog "remove? (y/n) " (T.pack path) "y" finish
    where finish path answer = handle (onError $ T.concat ["Cannot delete: ", path]) $ do
              when (isYes answer) $ do
                  lift $ Directory.removePathForcibly $ T.unpack path
                  reloadPanes
              modify (\s -> s { asDialog = Nothing })
          isYes a = let a' = T.toLower a
                    in  a' == "y" || a' == "yes"

appCreateDirectory :: App ()
appCreateDirectory = withDialog "mkdir" T.empty T.empty finish
    where finish _ dir = handle (onError $ T.concat ["Cannot create directory: ", dir]) $ do
              let path = T.unpack dir
              lift $ Directory.createDirectory path
              reloadPanes
              modify (\s -> s { asDialog = Nothing })

appSwitchActivePane :: App ()
appSwitchActivePane = do
    s <- get
    lift $ Directory.setCurrentDirectory (paneDirectory $ asOtherPane s)
    modify (\st -> st { asActivePane = (asOtherPane st)  { paneIsActive = True }
                      , asOtherPane  = (asActivePane st) { paneIsActive = False }
                      , asActiveLeft = not $ asActiveLeft st
                      })

appReloadActivePane :: App ()
appReloadActivePane = do
    entries <- lift $ Directory.getCurrentDirectory >>= loadEntries
    modify (\s -> s { asActivePane = (asActivePane s) { paneEntries = S.sort entries } })

helpWidth :: Num a => a
helpWidth = 24

helpHeight :: Num a => a
helpHeight = 20

appShowHelp :: App ()
appShowHelp = modify (\s -> s { asHelp = drawHelp })
  where
    drawHelp = Vty.text (Vty.defAttr `Vty.withStyle` Vty.bold) (T.center helpWidth ' ' "COMMANDS")
               <->
               foldr foldFunc Vty.emptyImage (fmap (T.justifyLeft helpWidth ' ')
                   [ "next entry:  down, k"
                   , "prev entry:  up, i"
                   , "page down:   pagedown"
                   , "page up:     pageup"
                   , "first entry: home"
                   , "change dir:  enter, g"
                   , "copy:        c"
                   , "move:        m"
                   , "delete:      x"
                   , "create dir:  d"
                   , "switch pane: tab"
                   , "reload pane: r"
                   , "help:        ?"
                   , "quit:        q"
                   ])
               <->
               Vty.text (Vty.defAttr `Vty.withStyle` Vty.bold) (T.center helpWidth ' ' "DIALOG")
               <->
               foldr foldFunc Vty.emptyImage (fmap (T.justifyLeft helpWidth ' ')
                   [ "finish:      enter"
                   , "cancel:      esc"
                   , "backspace:   backspace"
                   , "clear:       ctrl-u"
                   ])
    foldFunc l img = Vty.text Vty.defAttr l <-> img


app :: Vty.Vty -> App ()
app vty = do
    display vty
    ev <- lift $ Vty.nextEvent vty
    resolveKey ev =<< get
    s <- get
    unless (asQuit s) $ app vty

    where
     resolveKey :: Vty.Event -> AppState -> App ()
     resolveKey ev state
         | normal = case ev of
             Vty.EvKey (Vty.KChar 'k') [] -> appNextEntry
             Vty.EvKey Vty.KDown [] -> appNextEntry
             Vty.EvKey Vty.KPageDown [] -> replicateM_ getPaneHeight appNextEntry
             Vty.EvKey (Vty.KChar 'i') [] -> appPrevEntry
             Vty.EvKey Vty.KUp [] -> appPrevEntry
             Vty.EvKey Vty.KPageUp [] -> replicateM_ getPaneHeight appPrevEntry
             Vty.EvKey Vty.KHome [] -> appFirstEntry
             Vty.EvKey Vty.KEnter    [] -> appEnterEntry
             Vty.EvKey (Vty.KChar 'g') [] -> appChangeDirectory
             Vty.EvKey (Vty.KChar 'c') [] -> appCopyEntry
             Vty.EvKey (Vty.KChar 'm') [] -> appMoveEntry
             Vty.EvKey (Vty.KChar 'x') [] -> appDeleteEntry
             Vty.EvKey (Vty.KChar 'd') [] -> appCreateDirectory
             Vty.EvKey (Vty.KChar '\t') [] -> appSwitchActivePane
             Vty.EvKey (Vty.KChar 'r') [] -> appReloadActivePane
             Vty.EvKey (Vty.KChar '?') [] -> appShowHelp
             Vty.EvKey (Vty.KChar 'q') [] -> modify (\s -> s { asQuit = True })
             _ -> return ()
         | ondialog = case ev of
             Vty.EvKey Vty.KEnter [] -> dFinish $ fromJust $ asDialog state
             Vty.EvKey Vty.KEsc [] -> dCancel $ fromJust $ asDialog state
             Vty.EvKey Vty.KBS [] -> modify (\s -> s { asDialog = fmap backspace dlg })
             Vty.EvKey (Vty.KChar 'u') [Vty.MCtrl] -> modify (\s -> s { asDialog = fmap clear dlg })
             Vty.EvKey (Vty.KChar c) [] -> modify (\s -> s { asDialog = fmap (insertChar c) dlg })
             _ -> return ()
         | onerr = modify (\s -> s { asError = Nothing })
         | onhelp = modify (\s -> s { asHelp = Vty.emptyImage })
         | otherwise = error "Impossible"
         where err = asError state
               dlg = asDialog state
               normal = not onerr && not ondialog && not onhelp
               ondialog = isJust dlg
               onerr  = isJust err
               onhelp = asHelp state /= Vty.emptyImage

main :: IO ()
main = do
    vty <- Vty.mkVty Vty.defaultConfig
    pane <- loadPane =<< Directory.getCurrentDirectory
    _ <- execStateT (app vty) AppState { asActivePane = pane
                                       , asOtherPane = pane { paneIsActive = False }
                                       , asActiveLeft = True
                                       , asDialog = Nothing
                                       , asError = Nothing
                                       , asHelp = Vty.emptyImage
                                       , asQuit = False
                                       }
    Vty.shutdown vty

