import XMonad
import XMonad.Hooks.DynamicLog
import System.Exit
import XMonad.StackSet hiding (workspaces)
import Data.Map (Map, fromList)
import XMonad.Util.Run (safeSpawn)
import Graphics.X11.ExtraTypes.XF86
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers
import XMonad.Actions.CycleWS
import XMonad.Layout.CenteredMaster
import XMonad.Layout.Grid
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.MouseGestures
import XMonad.Util.EZConfig
import Control.Monad

toggleBarKey :: XConfig Layout -> (KeyMask, KeySym)
toggleBarKey XConfig {XMonad.modMask = modMask'} = (modMask', xK_b)

xconf :: XConfig (Choose Tall (Choose (ModifiedLayout CenteredMaster Grid) Full))
xconf = ewmh defaultConfig
      { modMask            = mod4Mask
      , terminal           = "xfce4-terminal"
      , borderWidth        = 1
      , normalBorderColor  = "#000000"
      , focusedBorderColor = "#000066"
      , layoutHook         = Tall 1 (3/100) (1/2) ||| centerMaster Grid ||| Full
      , manageHook         = manageHook'
      , startupHook        = return () >> checkKeymap xconf keymap
      , keys               = keys'
      , workspaces         = workspaces'
      , handleEventHook    = fullscreenEventHook
      , mouseBindings      = mouseBindings'
      } where
        workspaces' = ["main","web1"] ++ map (("dev" ++) . show) [1..6 :: Int] ++ ["game","chat","misc"]
        manageHook' = handleFullscreenFloat
          <+> manageHookShifts
          [ ("web1", ["Firefox"])
          , ("chat", ["Skype","Xchat"])
          , ("dev1", ["Eclipse"])
          , ("misc", ["Clementine","clementine","Steam"])
          ] <+> manageHookFloats []
        keys' conf = mkKeymap conf $ ("M-S-<Space>", setLayout $ layoutHook conf) : keymap

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

keymap :: [(String, X ())]
keymap = join [programs, audio, layouts, focus', shrinkAndExpand, recompileAndQuit, shifts, keyboardLayouts]
      where
        programs =
          [ ("M-<Return>", spawn $ terminal xconf)
          , ("M-p"       , safeSpawn "dmenu_run" [])
          , ("M-S-c"     , kill)
          , ("M-w"       , safeSpawn "xtrlock" [])
          ]
        audio =
          [ ("<XF86AudioRaiseVolume>", safeSpawn "amixer" ["-q", "set", "Master", "1+"])
          , ("<XF86AudioLowerVolume>", safeSpawn "amixer" ["-q", "set", "Master", "1-"])
          , ("<XF86AudioMute>"       , safeSpawn "amixer" ["-q", "set", "Master", "toggle"])
          , ("<XF86AudioPlay>"       , safeSpawn "mpc" ["toggle"])
          , ("<XF86AudioNext>"       , safeSpawn "mpc" ["next"])
          , ("<XF86AudioPrev>"       , safeSpawn "mpc" ["prev"])
          ]
        layouts =
          [ ("M-<Space>"  , sendMessage NextLayout)
      --  , ("M-S-<Space>", setLayout $ layoutHook conf)
          , ("M-t"        , withFocused $ windows . sink)
          , ("M-n"        , refresh)
          ]
        focus' =
          [ ("M-S-<Tab>"   , windows focusUp)
          , ("M-<Tab>"     , windows focusDown)
          , ("M-m"         , windows focusMaster)
          , ("M-S-<Return>", windows swapMaster)
          , ("M-<Right>"   , nextWS)
          , ("M-<Left>"    , prevWS)
          , ("M-j"         , prevScreen)
          , ("M-k"         , nextScreen)
          ]
        shrinkAndExpand =
          [ ("M-,", sendMessage (IncMasterN 1))
          , ("M-.", sendMessage (IncMasterN (-1)))
          , ("M-h", sendMessage Shrink)
          , ("M-l", sendMessage Expand)
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
          [ ("M-<F1>"  , spawn "setxkbmap -layout dvp")
          , ("M-S-<F1>", spawn "setxkbmap -layout de")
          ]

xmobarpp :: PP
xmobarpp = xmobarPP
  { ppSep = " "
  , ppOrder = \(ws:_:t:_) -> [ws,t]
  , ppTitle = formatTitle
  } where
    formatTitle :: String -> String
    formatTitle str = let wntitle = shorten 40 str in
      replicate (40 - length wntitle) ' '
      ++ xmobarColor "green" "black" (wrap "<<" ">>" $ xmobarColor "grey" "black" wntitle)

main :: IO ()
main = do
  let uhook = withUrgencyHookC NoUrgencyHook UrgencyConfig { suppressWhen = Focused, remindWhen = Dont }
  statBar <- statusBar "xmobar" xmobarpp toggleBarKey $ uhook xconf
  xmonad statBar
