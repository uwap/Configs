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
      , startupHook        = return ()
      , keys               = keys'
      , workspaces         = workspaces'
      , handleEventHook    = fullscreenEventHook
      } where
        workspaces' = ["main","web1"] ++ map (("dev" ++) . show) [1..6 :: Int] ++ ["game","chat","misc"]
        manageHook' = handleFullscreenFloat
          <+> manageHookShifts
          [ ("web1", ["Firefox"])
          , ("chat", ["Skype","Xchat"])
          , ("dev1", ["Eclipse"])
          , ("misc", ["Clementine","clementine","Steam"])
          ] <+> manageHookFloats []

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

keys' :: XConfig Layout -> Map (KeyMask, KeySym) (X ())
keys' conf@XConfig {XMonad.modMask = modMask'} = fromList . concat $
  [programs, audio, layouts, focus', shrinkAndExpand, recompileAndQuit, shifts, keyboardLayouts]
      where
        programs :: [((KeyMask, KeySym), X ())]
        programs = let modMaskS = modMask' .|. shiftMask in
          [ ((modMask', xK_Return), spawn $ terminal conf)
          , ((modMask', xK_p     ), safeSpawn "dmenu_run" [])
          , ((modMaskS, xK_c     ), kill)
          , ((modMask', xK_w     ), safeSpawn "xtrlock" [])
          ]
        audio :: [((KeyMask, KeySym), X ())]
        audio =
          [ ((0, xF86XK_AudioRaiseVolume), safeSpawn "amixer" ["-q", "set", "Master", "1+"])
          , ((0, xF86XK_AudioLowerVolume), safeSpawn "amixer" ["-q", "set", "Master", "1-"])
          , ((0, xF86XK_AudioMute       ), safeSpawn "amixer" ["-q", "set", "Master", "toggle"])
          , ((0, xF86XK_AudioPlay       ), safeSpawn "mpc" ["toggle"])
          , ((0, xF86XK_AudioNext       ), safeSpawn "mpc" ["next"])
          , ((0, xF86XK_AudioPrev       ), safeSpawn "mpc" ["prev"])
          ]
        layouts :: [((KeyMask, KeySym), X ())]
        layouts = let modMaskS = modMask' .|. shiftMask in
          [ ((modMask', xK_space ), sendMessage NextLayout)
          , ((modMaskS, xK_space ), setLayout $ layoutHook conf)
          , ((modMask', xK_t     ), withFocused $ windows . sink)
          , ((modMask', xK_n     ), refresh)
          ]
        focus' :: [((KeyMask, KeySym), X ())]
        focus' = let modMaskS = modMask' .|. shiftMask in 
          [ ((modMaskS, xK_Tab   ), windows focusUp)
          , ((modMask', xK_Tab   ), windows focusDown)
          , ((modMask', xK_m     ), windows focusMaster)
          , ((modMaskS, xK_Return), windows swapMaster)
          , ((modMask', xK_Right ), nextWS)
          , ((modMask', xK_Left  ), prevWS)
          , ((modMask', xK_j     ), prevScreen)
          , ((modMask', xK_k     ), nextScreen)
          ]
        shrinkAndExpand :: [((KeyMask, KeySym), X ())]
        shrinkAndExpand =
          [ ((modMask', xK_comma ), sendMessage (IncMasterN 1))
          , ((modMask', xK_period), sendMessage (IncMasterN (-1)))
          , ((modMask', xK_h     ), sendMessage Shrink)
          , ((modMask', xK_l     ), sendMessage Expand)
          ]
        recompileAndQuit :: [((KeyMask, KeySym), X ())]
        recompileAndQuit = let modMaskS = modMask' .|. shiftMask in
          [ ((modMaskS, xK_q      ), io exitSuccess)
          , ((modMask', xK_q      ), spawn "xmonad --recompile; xmonad --restart")
          ]
        shifts :: [((KeyMask, KeySym), X ())]
        shifts = let modMaskS = modMask' .|. shiftMask in
          [ ((modMaskS, xK_Right), shiftToNext)
          , ((modMaskS, xK_Left ), shiftToPrev)
          , ((modMaskS, xK_j    ), shiftPrevScreen)
          , ((modMaskS, xK_k    ), shiftNextScreen)
          ]
        keyboardLayouts :: [((KeyMask, KeySym), X ())]
        keyboardLayouts = let modMaskS = modMask' .|. shiftMask in
          [ ((modMask', xK_F1), spawn "setxkbmap -layout dvp")
          , ((modMaskS, xK_F1), spawn "setxkbmap -layout de")
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
