{-# LANGUAGE DeriveDataTypeable, LambdaCase, MultiWayIf #-}
import XMonad
import XMonad.Hooks.DynamicLog
import System.Exit
import XMonad.StackSet hiding (workspaces)
import Data.Map (Map, fromList)
import Data.List (isInfixOf, isSuffixOf)
import Data.Char (toLower)
import XMonad.Util.Run
import Graphics.X11.ExtraTypes.XF86
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Actions.CycleWS
import XMonad.Layout.CenteredMaster
import XMonad.Layout.Grid
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.WindowNavigation
import XMonad.Layout.SubLayouts
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Tabbed
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.MouseGestures
import XMonad.Actions.Submap
import XMonad.Actions.Navigation2D
import XMonad.Util.EZConfig
import Control.Monad
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Prompt.XMonad
import XMonad.Prompt.Input
import XMonad.Prompt.Pass
import XMonad.Hooks.FadeInactive

import XMonad.Layout.Decoration
import XMonad.Util.Font
import qualified XMonad.Util.Dzen as Dzen

import qualified XMonad.Util.ExtensibleState as XS

data KeyModes = Normal | Window deriving (Typeable, Show)
instance ExtensionClass KeyModes where initialValue = Normal

toggleBarKey :: XConfig Layout -> (KeyMask, KeySym)
toggleBarKey XConfig {XMonad.modMask = modMask'} = (modMask', xK_b)

xconf = do
    ewmh defaultConfig
      { modMask            = mod4Mask
      , terminal           = "xfce4-terminal --hide-menubar"
      , borderWidth        = 2
      , normalBorderColor  = "#2469a5"
      , focusedBorderColor = "#e0116e"
      , layoutHook         = myLayout
      , manageHook         = manageHook'
      , startupHook        = return () >> checkKeymap xconf keymap
      , keys               = keys'
      , workspaces         = workspaces'
      , handleEventHook    = fullscreenEventHook
      , mouseBindings      = mouseBindings'
     -- , logHook            = fadeInactiveLogHook 0.8
      } where
        workspaces' = ["misc","web1"] ++ map (("dev" ++) . show) [1..6 :: Int] ++ ["chat","game"]
        manageHook' = handleFullscreenFloat
          <+> manageHookShifts
          [ ("web1", ["Firefox"])
          , ("chat", ["Skype","Xchat","mail","Mail","Thunderbird"])
          , ("dev1", ["Eclipse"])
          , ("game", ["Steam"])
          ] <+> manageHookFloats []
        keys' conf = mkKeymap conf $ ("M-S-<Space>", setLayout $ layoutHook conf) : keymap

myLayout = avoidStruts $ smartSpacingWithEdge 7
    $ windowNavigation
    $ Full ||| ThreeColMid 1 (3/100) (1/2) ||| ThreeCol 1 (3/100) (1/2)
  where
    titleTheme = def
      { activeBorderColor = "#AAAAFF"
      , inactiveBorderColor = "#7777FF"
      , urgentBorderColor = "#FF7777"
      , decoHeight = 18
      , decoWidth = 100
      }

manageHookShifts :: [(String, [String])] -> ManageHook
manageHookShifts = composeAll . concatMap (uncurry shiftToWS)
  where
    shiftToWS :: String -> [String] -> [ManageHook]
    shiftToWS ws apps = [className =? s --> doF (shift ws) | s <- apps]

manageHookFloats :: [String] -> ManageHook
manageHookFloats = composeAll . map doFloat'
  where
    doFloat' :: String -> ManageHook
    doFloat' s = className =? s --> doFloat

handleFullscreenFloat :: ManageHook
handleFullscreenFloat = isFullscreen --> doFloat

mouseBindings' :: XConfig Layout -> Map (KeyMask, Button) (Window -> X ())
mouseBindings' conf@XConfig {XMonad.modMask = modMask'} =
  fromList [ ((modMask' .|. shiftMask, button1), mouseGesture gestures) ]
  <+> mouseBindings defaultConfig conf

gestures :: Map [Direction2D] (Window -> X ())
gestures = fromList [ ([L],   const nextWS)
                    , ([R],   const prevWS)
                    , ([R,U], const shiftToNext)
                    , ([L,U], const shiftToPrev)
                    , ([D],   const kill)
                    ]

xprompt :: XPConfig
xprompt = def { searchPredicate = isInfixOf
              , bgColor = "#1B1D1E"
              , fgColor = "#ACACAC"
              , fgHLight = "#ebac54"
              , bgHLight = "#1B1D1E"
              , borderColor = "#1A1C1D"
              , position = CenteredAt 0.5 1
              , font = "-*-*-*-*-*-*-16-*-*-*-*-*-*-*"
              , height = 24
              }

keymap :: [(String, X ())]
keymap = join [programs, audio, layouts, focus', shrinkAndExpand, recompileAndQuit, shifts, keyboardLayouts, screensaver]
      where
        programs =
          [ ("M-<Return>", spawn $ terminal xconf)
          , ("M-p"       , shellPrompt xprompt)
          , ("M-S-p"     , windowPromptBring xprompt)
          , ("M-y"       , passPrompt xprompt)
          , ("M-S-y"     , passGeneratePrompt xprompt)
       --   , ("M-o"       , windowPromptBring xprompt)
          , ("M-S-o"     , xmonadPrompt xprompt)
          , ("M-S-c"     , kill)
          , ("M-w"       , spawn "sleep .1; xtrlock-pam")
          , ("M-,"       , spawn "sleep .1; xtrlock-pam")
          ]
        audio =
          [ ("<XF86AudioRaiseVolume>", safeSpawn "amixer" ["-q", "-D", "pulse", "sset", "Master", "1%+"])
          , ("<XF86AudioLowerVolume>", safeSpawn "amixer" ["-q", "-D", "pulse", "sset", "Master", "1%-"])
          , ("<XF86AudioMute>"       , safeSpawn "amixer" ["-q", "-D", "pulse", "sset", "Master", "toggle"])
          , ("<XF86AudioPlay>"       , safeSpawn "vlc" ["--key-play-pause"])--safeSpawn "mpc" ["toggle"])
          , ("<XF86AudioNext>"       , safeSpawn "mpc" ["next"])
          , ("<XF86AudioPrev>"       , safeSpawn "mpc" ["prev"])
          ]
        layouts =
          [ ("M-<Space>"  , sendMessage NextLayout)
      --  , ("M-S-<Space>", setLayout $ layoutHook conf)
          , ("M-t"        , withFocused $ windows . sink)
          , ("M-n"        , refresh)

          , ("M-s"        , asks config >>= submap . defaultSublMap)
          , ("M-i"        , XS.get >>= \case
                    Normal -> sendMessage $ pushGroup U
                    Window -> windowGo U True)
          , ("M-j"        , XS.get >>= \case
                    Normal -> sendMessage $ pushGroup L
                    Window -> windowGo L True)
          , ("M-k"        , XS.get >>= \case
                    Normal -> sendMessage $ pushGroup D
                    Window -> windowGo D True)
          , ("M-l"        , XS.get >>= \case
                    Normal -> sendMessage $ pushGroup R
                    Window -> windowGo R True)
          , ("M-S-v"      , withFocused (sendMessage . MergeAll))
          , ("M-S-b"      , withFocused (sendMessage . UnMerge))
          , ("M-b"        , sendMessage ToggleStruts)
          ]
        focus' =
          [ ("M-S-<Tab>"   , windows focusUp)
          , ("M-<Tab>"     , windows focusDown)
          , ("M-m"         , windows focusMaster)
          , ("M-S-<Return>", windows swapMaster)
          , ("M-<Right>"   , XS.get >>= \x -> case x of
                              Normal -> nextWS
                              Window -> windows swapMaster)
          , ("M-<Left>"    , prevWS)
          , ("M-u"         , prevScreen)
          , ("M-o"         , nextScreen)
          , ("M-e"         , XS.put Normal)
          , ("M-S-e"       , XS.put Window)
          ]
        shrinkAndExpand =
      --    [ ("M-,", sendMessage (IncMasterN 1))
      -- , ("M-.", sendMessage (IncMasterN (-1)))
          [ ("M-h", sendMessage Shrink)
          , ("M-z", sendMessage Expand)
          ]
        recompileAndQuit =
          [ ("M-S-q", io exitSuccess)
          , ("M-q"  , spawn "xmonad --recompile; xmonad --restart")
          ]
        shifts =
          [ ("M-S-<Right>", shiftToNext)
          , ("M-S-<Left>" , shiftToPrev)
          , ("M-S-j"      , shiftPrevScreen)
          , ("M-S-k"      , shiftNextScreen)
          ]
        keyboardLayouts =
          [ ("M-<F1>"  , spawn "setxkbmap -layout us; setxkbmap -variant dvp")
          , ("M-S-<F1>", spawn "setxkbmap -layout de")
          ]
        screensaver =
          [ ("M-<F2>"  , ynInput "Disable screen saving? [Y/N]" $ spawn "xset s off; xset -dpms")
          , ("M-S-<F2>", ynInput "Enable screen saving? [Y/N]"  $ spawn "xset s on; xset +dpms")
          ]
        ynInput str action = inputPrompt xprompt str ?+ \x -> when ("y" `isInfixOf` map toLower x || "j" `isInfixOf` map toLower x) action

dzenpp :: PP
dzenpp = dzenPP
  { ppSep = ""
  , ppOrder = \p -> case p of
      ws:s:xs -> s:ws:xs
      xs -> xs
  , ppLayout = formatLayout
  , ppCurrent         = dzenColor "#ebac54" "" . (++ " ")
  , ppVisible         = dzenColor "#ACACAC" "" . (++ " ")
  , ppHidden          = dzenColor "white" "" . (++ " ")
  , ppHiddenNoWindows = dzenColor "#7b7b7b" "" . (++ " ")
  , ppUrgent          = dzenColor "#eb7254" "" . (++ " ")
  , ppTitle           = (" " ++) . dzenColor "#ACACAC" "" . dzenEscape
 -- , ppOrder = \(ws:a:t:m:xs) -> ws:a:t:m:xs
--  , ppTitle = formatTitle
--  , ppHidden = xmobarColor "white" "grey"
--  , ppCurrent = xmobarColor "white" "black"
 -- , ppExtras = [Just . show <$> (XS.get :: X KeyModes)] 
  } where
      formatLayout x = dzenColor "#ebac54" "#1B1D1E" $
        " ^ca(1, xdotool keydown super key space keyup super)" ++ layoutImage x ++ "^ca() "
      layoutImage x = case x of
        _ | "ThreeCol" `isSuffixOf` x -> "^i(/home/uwap/.xmonad/dzen2/three.xbm)"
        _ | "Full" `isSuffixOf` x -> "^i(/home/uwap/.xmonad/dzen2/full.xbm)"
        _ | otherwise -> x

main :: IO ()
main = do
  let uhook = withUrgencyHookC NoUrgencyHook UrgencyConfig { suppressWhen = Focused, remindWhen = Dont }
  let dzenLeft = "dzen2 -dock -x '0' -y '0' -w '1000' -ta 'l' -h 24 -fn '-*-sans-*-*-*-*-*-*-*-*-*-*-*-*'"
  let dzenRight = "conky -c /home/uwap/.xmonad/.conky_dzen | dzen2 -dock -x '1000' -w '920' -ta 'r' -y '0' -h 24 -fn '-*-sans-*-*-*-*-*-*-*-*-*-*-*-*'"
  spawnPipe dzenRight
  spawn "while true; do feh --bg-scale ~/Bilder/wallpaper/ --randomize; sleep 3000; done"
  statBar <- statusBar dzenLeft dzenpp toggleBarKey $ uhook xconf
  xmonad statBar
