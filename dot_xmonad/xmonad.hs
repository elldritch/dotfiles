module Main (main) where

import Control.Monad (liftM2)
import Data.List (elemIndex, findIndex, isSuffixOf)
import Data.List.Extra (split, trim)
import Data.Set (member)
import Graphics.X11.ExtraTypes.XF86
import Relude
import Relude.Unsafe (fromJust)
import XMonad
import XMonad.Actions.DynamicWorkspaces (addHiddenWorkspace, addWorkspace, removeEmptyWorkspace)
import XMonad.Actions.PhysicalScreens (sendToScreen, viewScreen)
import XMonad.Hooks.DynamicLog (PP (..), dynamicLogWithPP, shorten, xmobarColor, xmobarPP)
import XMonad.Hooks.ManageDocks (ToggleStruts (..), avoidStruts, docks, manageDocks)
import XMonad.Hooks.Place (placeHook, simpleSmart)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Layout.NoBorders (smartBorders)
import qualified XMonad.StackSet as SS
import XMonad.Util.Dmenu (dmenu)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Loggers (Logger)
import XMonad.Util.NamedWindows (getName, unName)
import XMonad.Util.Run (hPutStrLn, runProcessWithInput, spawnPipe)
import qualified Prelude

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ docks $ xConf xmproc `additionalKeys` keybindings

-- Logs to `~/.xsession-errors`
debug :: (MonadIO m) => String -> m ()
debug = liftIO . hPutStrLn stderr

spawnDebug :: (MonadIO m) => String -> String -> m String
spawnDebug msg cmd = do
  debug msg
  debug $ "running command: " ++ show cmd
  output <- liftIO $ spawnOutput cmd
  debug $ "command output: " ++ show output
  return output

spawnOutput :: (MonadIO m) => String -> m String
spawnOutput s = runProcessWithInput "/bin/sh" ["-c", s] ""

-- XMonad configuration
xConf outputHandle =
  def
    { borderWidth = 2,
      modMask = superMask,
      focusFollowsMouse = False,
      clickJustFocuses = False,
      terminal = "terminator",
      workspaces = snd <$> customWorkspaces,
      -- Workaround for Java Swing applications: https://stackoverflow.com/questions/30742662/java-swing-gui-not-displaying-in-xmonad
      startupHook = setWMName "LG3D",
      manageHook = manageDocks <+> placeHook simpleSmart <+> manageHook def,
      layoutHook = avoidStruts $ smartBorders $ layoutHook def,
      logHook =
        dynamicLogWithPP
          xmobarPP
            { ppOutput = hPutStrLn outputHandle,
              ppTitle = xmobarColor "green" "",
              ppExtras = [logTitles (xmobarColor "green" "")],
              ppOrder = \case
                (ws : l : _ : e) -> [ws, l] ++ e
                _ -> error "impossible: xmobar pretty-print args malformed"
            }
    }

-- Extra workspaces
customWorkspaces :: [(KeySym, WorkspaceId)]
customWorkspaces =
  zip
    ([xK_grave] ++ [xK_1 .. xK_9] ++ [xK_0, xK_minus, xK_equal, xK_BackSpace])
    ((["~"] ++ fmap show [1 .. 9 :: Int]) ++ ["0", "-", "=", "B"])

customWorkspaceNames :: [WorkspaceId]
customWorkspaceNames = snd <$> customWorkspaces

customWorkspaceNamesSet :: Set WorkspaceId
customWorkspaceNamesSet = fromList customWorkspaceNames

dmenuWorkspaces :: (WorkspaceId -> X ()) -> X ()
dmenuWorkspaces f = do
  wss <- withWindowSet (return . SS.workspaces)
  let names = fmap SS.tag wss
  ws <-
    dmenu $
      filter (not . null) $
        customWorkspaceNames
          ++ sort (filter (\x -> not $ member x customWorkspaceNamesSet) names)
  if null ws then return () else f ws

-- Log truncated pipe-delimited window titles, highlighting focused window
logTitles :: (String -> String) -> Logger
logTitles ppFocus =
  withWindowSet $ fmap (Just . intercalate " | ") . windowTitles
  where
    windowName = shorten 50 . show
    isFocused windowSet target = (== Just (unName target)) $ SS.peek windowSet
    highlightFocused windowSet target =
      (if isFocused windowSet target then ppFocus else id) $ windowName target
    namedWindows = fmap getName . SS.index
    windowTitles windowSet =
      mapM (fmap $ highlightFocused windowSet) $ namedWindows windowSet

-- Custom keybindings
type Keybinding = ((KeyMask, KeySym), X ())

leftAltMask :: KeyMask
leftAltMask = mod1Mask

-- https://unix.stackexchange.com/questions/238877/can-xmonad-treat-left-and-right-alt-differently
-- https://unix.stackexchange.com/questions/239988/why-isnt-x-treating-alt-l-and-alt-r-differently-w-r-t-mod1
rightAltMask :: KeyMask
rightAltMask = mod3Mask

superMask :: KeyMask
superMask = mod4Mask

defaultMasks :: [KeyMask]
defaultMasks = [superMask, rightAltMask]

hotkey :: KeySym -> String -> [Keybinding]
hotkey key command = hotkey' key $ spawn command

hotkey' :: KeySym -> X () -> [Keybinding]
hotkey' key action = [((noModMask, key), action)]

withMask :: KeySym -> String -> [Keybinding]
withMask key command = withMask' key $ spawn command

withMask' :: KeySym -> X () -> [Keybinding]
withMask' key action = [((mask, key), action) | mask <- defaultMasks]

withSMask :: KeySym -> String -> [Keybinding]
withSMask key command = withSMask' key $ spawn command

withSMask' :: KeySym -> X () -> [Keybinding]
withSMask' key action = [((mask .|. shiftMask, key), action) | mask <- defaultMasks]

keybindings :: [Keybinding]
keybindings =
  concat [ -- Custom kill
    withSMask xK_c "xkill",
    -- Close window
    withMask' xK_c kill,
    -- Lock screen
    withSMask xK_l "slock",
    withSMask xK_u "slock dm-tool lock",
    -- Screen brightness
    hotkey xF86XK_MonBrightnessUp "xbacklight -inc 10",
    hotkey xF86XK_MonBrightnessDown "xbacklight -dec 10",
    -- Volume control with media keys
    hotkey xF86XK_AudioMute "amixer sset Master toggle",
    hotkey xF86XK_AudioLowerVolume "amixer sset Master 1%-",
    hotkey xF86XK_AudioRaiseVolume "amixer sset Master 1%+",
    -- Microphone control with media keys
    hotkey xF86XK_AudioMicMute "amixer sset Capture toggle",
    -- Volume control with function keys
    withMask xK_F10 "amixer sset Master toggle",
    withMask xK_F11 "amixer sset Master 1%-",
    withMask xK_F12 "amixer sset Master 1%+",
    -- Microphone control with function keys
    withMask xK_F9 "amixer sset Capture toggle",
    -- Select audio output with dmenu (TODO: refactor action?)
    withSMask' xK_F10 $ do
      sinks <- getSinks
      sinkName <- dmenu $ name <$> sinks
      if null sinkName
        then return ()
        else do
          let sink = fromJust $ find (\s -> name s == sinkName) sinks
          -- Query for the card profile of the sink.
          cards <- getCards
          let sinkCard = fromJust $ find (\c -> alsaCardName c == card sink) cards
          profile <- dmenu $ profiles sinkCard
          if null profile
            then return ()
            else do
              -- Get current sink inputs and move them to the new sink.
              sinkInputs <- Prelude.lines <$> spawnOutput "pactl list short sink-inputs | awk '{print $1}'"
              mapM_ (\sinkInput -> spawn $ "pactl move-sink-input " ++ sinkInput ++ " " ++ sinkName) sinkInputs
              -- Set the card profile.
              spawn $ "pactl set-card-profile " ++ pulseAudioCardName sinkCard ++ " " ++ profile
              -- Set the default sink (this is what the ALSA volume commands adjust).
              spawn $ "pactl set-default-sink " ++ sinkName,
    -- Media player controls with numpad top row
    withMask xK_KP_Divide "playerctl previous",
    withMask xK_KP_Multiply "playerctl play-pause",
    withMask xK_KP_Subtract "playerctl next",
    -- Media player controls with arrow keys
    withMask xK_Left "playerctl previous",
    withMask xK_Up "playerctl play-pause",
    withMask xK_Down "playerctl stop",
    withMask xK_Right "playerctl next",
    -- Custom launcher (see `~/.profile` re: $PATH)
    withMask xK_p "$(yeganesh -x)",
    -- Dynamic workspaces
    withMask' xK_backslash $ dmenuWorkspaces addWorkspace,
    withSMask' xK_backslash $ dmenuWorkspaces (liftM2 (>>) addHiddenWorkspace (windows . SS.shift)),
    withMask' xK_Delete removeEmptyWorkspace,
    -- Screenshots
    hotkey xK_Print "maim ~/screenshots/$(date +%s).png",
    withMask xK_Print "maim -s ~/screenshots/$(date +%s).png",
    -- Remap reload => mask-r and physical screens to mask-{q,w,e}
    withMask' xK_r $ do
      ok <- recompile False
      if ok then restart "xmonad" True else spawn "xmessage recompile failed",
    withSMask' xK_r $ io exitSuccess,
    -- Clipboard manager
    withMask xK_v "gpaste-client ui",
    -- Toggle struts (xmobar visibility)
    withMask' xK_b $ sendMessage ToggleStruts
  ]
    ++
    -- Workspaces
    [ ((mask .|. extraMask, key), windows $ action workspace)
      | (key, workspace) <- customWorkspaces,
        (action, extraMask) <- [(SS.greedyView, noModMask), (SS.shift, shiftMask)],
        mask <- defaultMasks
    ]
    ++
    -- Physical monitors (triple-headed setup)
    [ ((mask .|. extraMask, key), action def screen)
      | (key, screen) <- zip [xK_q, xK_w, xK_e] [0 ..],
        (action, extraMask) <- [(viewScreen, noModMask), (sendToScreen, shiftMask)],
        mask <- defaultMasks
    ]

-- TODO: we should probably split this into a separate module.
data PulseAudioSink = PulseAudioSink
  { name :: String,
    card :: String
  }
  deriving (Show)

data PulseAudioCard = PulseAudioCard
  { pulseAudioCardName :: String,
    alsaCardName :: String,
    profiles :: [String]
  }
  deriving (Show)

getCards :: (MonadIO m) => m [PulseAudioCard]
getCards = do
  result <- spawnOutput "pactl list cards"
  return $
    linesToCard
      <$> filter
        (not . null)
        (split (\s -> "Card " `isPrefixOf` s) $ trim <$> Prelude.lines result)
  where
    linesToCard :: [String] -> PulseAudioCard
    linesToCard ls =
      PulseAudioCard
        { pulseAudioCardName = fromPrefixedLine "Name: " ls,
          alsaCardName = fromPrefixedLine "alsa.card = \"" $ Prelude.init <$> ls,
          profiles = rights $ extractProfile <$> availableProfiles
        }
      where
        profilesStart = fromJust $ elemIndex "Profiles:" ls
        profilesEnd = fromJust $ findIndex (\s -> "Active Profile: " `isPrefixOf` s) ls
        profileLines = drop (profilesStart + 1) $ take profilesEnd ls
        availableProfiles = filter (\s -> "available: yes)" `isSuffixOf` s) profileLines
        extractProfile :: String -> Either String String
        extractProfile line =
          if "output:" `isPrefixOf` profileName
            then Right $ Prelude.init profileName
            else Left line
          where
            profileName = Prelude.head $ split (== ' ') line

-- TODO: these should probably have real parsers.
getSinks :: (MonadIO m) => m [PulseAudioSink]
getSinks = do
  result <- spawnOutput "pactl list sinks"
  return $
    linesToSink
      <$> filter
        (not . null)
        (split (\s -> "Sink " `isPrefixOf` s) $ trim <$> Prelude.lines result)
  where
    linesToSink :: [String] -> PulseAudioSink
    linesToSink ls =
      PulseAudioSink
        { name = fromPrefixedLine "Name: " ls,
          card = fromPrefixedLine "alsa.card = \"" $ Prelude.init <$> ls
        }

fromPrefixedLine :: String -> [String] -> String
fromPrefixedLine prefix ls = Prelude.head $ catMaybes $ matchLineWithPrefix prefix <$> ls

matchLineWithPrefix :: String -> String -> Maybe String
matchLineWithPrefix prefix l =
  if prefix `isPrefixOf` l
    then Just (drop (length prefix) l)
    else Nothing
