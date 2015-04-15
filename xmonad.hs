import XMonad
import XMonad.Hooks.DynamicLog
import System.Exit
import XMonad.StackSet hiding (workspaces)
import Data.Map (Map, fromList)
import XMonad.Util.Run (safeSpawn)
import Graphics.X11.ExtraTypes.XF86
import XMonad.Hooks.UrgencyHook
import XMonad.Actions.CycleWS
import XMonad.Layout.CenteredMaster
import XMonad.Layout.Grid
import XMonad.Layout.LayoutModifier (ModifiedLayout)

toggleBarKey :: XConfig Layout -> (KeyMask, KeySym)
toggleBarKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

conf :: XConfig (Choose Tall (Choose (ModifiedLayout CenteredMaster Grid) Full))
conf = defaultConfig { modMask            = mod4Mask
                     , terminal           = "xfce4-terminal"
                     , borderWidth        = 1
                     , normalBorderColor  = "#000000"
                     , focusedBorderColor = "#000066"
                     , layoutHook         = Tall 1 (3/100) (1/2) ||| centerMaster Grid ||| Full
                     , manageHook         = manageHook'
                     , startupHook        = startupHook'
                     , keys               = keys'
                     , workspaces         = workspaces'
                     }
                where
                  workspaces' = ["main","web1"] ++ map (("dev" ++) . show) [1..6] ++ ["game","chat","misc"]
                  startupHook' = return ()
                  manageHook' = manageHookComposer
                    [ ("web1", ["Firefox"])
                    , ("chat", ["Skype","Xchat"])
                    , ("dev1", ["Eclipse"])
                    , ("misc", ["Clementine","clementine","Steam"])
                    ]

manageHookComposer :: [(String, [String])] -> ManageHook
manageHookComposer = composeAll . concatMap doShift
                where
                  doShift :: (String, [String]) -> [ManageHook]
                  doShift (ws,apps) = [className =? s --> doF (shift ws) | s <- apps]

keys' :: XConfig Layout -> Map (KeyMask, KeySym) (X ())
keys' conf@(XConfig {XMonad.modMask = modMask}) = fromList . concat $
    sequence (sequence [programs, audio, layouts, focus, shrinkAndExpand, recompileAndQuit, shifts] modMask) conf
      where
        programs :: KeyMask -> XConfig Layout -> [((KeyMask, KeySym), X ())]
        programs modMask conf = let modMaskS = modMask .|. shiftMask in
          [ ((modMask , xK_Return), spawn $ terminal conf)
          , ((modMask , xK_p     ), safeSpawn "dmenu_run" [])
          , ((modMaskS, xK_c     ), kill)
          , ((modMask , xK_l     ), safeSpawn "xscreensaver-command" ["--lock"])
          ]
        audio :: KeyMask -> XConfig Layout -> [((KeyMask, KeySym), X ())]
        audio modMask conf =
          [ ((0, xF86XK_AudioRaiseVolume), safeSpawn "amixer" ["-q", "set", "Master", "1+"])
          , ((0, xF86XK_AudioLowerVolume), safeSpawn "amixer" ["-q", "set", "Master", "1-"])
          , ((0, xF86XK_AudioMute       ), safeSpawn "amixer" ["-q", "set", "Master", "toggle"])
          -- use MPD instead of clementine when raspberry pi setup is ready
          , ((0, xF86XK_AudioPlay       ), safeSpawn "clementine" ["-t"])
          , ((0, xF86XK_AudioNext       ), safeSpawn "clementine" ["-f"])
          , ((0, xF86XK_AudioPrev       ), safeSpawn "clementine" ["-r"])
          ]
        layouts :: KeyMask -> XConfig Layout -> [((KeyMask, KeySym), X ())]
        layouts modMask conf = let modMaskS = modMask .|. shiftMask in
          [ ((modMask , xK_space ), sendMessage NextLayout)
          , ((modMaskS, xK_space ), setLayout $ layoutHook conf)
          , ((modMask , xK_t     ), withFocused $ windows . sink)
          , ((modMask , xK_n     ), refresh)
          ]
        focus :: KeyMask -> XConfig Layout -> [((KeyMask, KeySym), X ())]
        focus modMask conf = let modMaskS = modMask .|. shiftMask in 
          [ ((modMaskS, xK_Tab   ), windows focusUp)
          , ((modMask , xK_Tab   ), windows focusDown)
          , ((modMask , xK_m     ), windows focusMaster)
          , ((modMaskS, xK_Return), windows swapMaster)
          , ((modMask , xK_Right ), nextWS)
          , ((modMask , xK_Left  ), prevWS)
          , ((modMask , xK_j     ), prevScreen)
          , ((modMask , xK_k     ), nextScreen)
          ]
        shrinkAndExpand :: KeyMask -> XConfig Layout -> [((KeyMask, KeySym), X ())]
        shrinkAndExpand modMask conf =
          [ ((modMask , xK_comma ), sendMessage (IncMasterN 1))
          , ((modMask , xK_period), sendMessage (IncMasterN (-1)))
          , ((modMask , xK_h     ), sendMessage Shrink)
          , ((modMask , xK_l     ), sendMessage Expand)
          ]
        recompileAndQuit :: KeyMask -> XConfig Layout -> [((KeyMask, KeySym), X ())]
        recompileAndQuit modMask conf = let modMaskS = modMask .|. shiftMask in
          [ ((modMaskS, xK_q      ), io exitSuccess)
          , ((modMask , xK_q      ), spawn "xmonad --recompile; xmonad --restart")
          ]
        shifts :: KeyMask -> XConfig Layout -> [((KeyMask, KeySym), X ())]
        shifts modMask conf = let modMaskS = modMask .|. shiftMask in
          [ ((modMaskS, xK_Right), shiftToNext)
          , ((modMaskS, xK_Left ), shiftToPrev)
          , ((modMaskS, xK_j    ), shiftPrevScreen)
          , ((modMaskS, xK_k    ), shiftNextScreen)
          ]

main :: IO ()
main = do
  let uhook    = withUrgencyHookC NoUrgencyHook UrgencyConfig { suppressWhen = Focused, remindWhen = Dont }
  let xmobarpp = xmobarPP { ppOrder = \(ws:_) -> [ws] }
  statBar <- statusBar "xmobar" xmobarpp toggleBarKey $ uhook conf
  xmonad statBar