import System.IO (Handle, hPutStrLn)
import System.Exit
import XMonad
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.Minimize
import XMonad.Hooks.ManageHelpers(doFullFloat, doCenterFloat, isFullscreen, isDialog)
import XMonad.Config.Desktop
import XMonad.Config.Dmwit(viewShift)
import XMonad.Util.Run(spawnPipe)
import XMonad.Actions.SpawnOn
import XMonad.Util.EZConfig (additionalKeys, additionalMouseBindings)
import XMonad.Actions.CycleWS
import XMonad.Hooks.UrgencyHook
import qualified Codec.Binary.UTF8.String as UTF8
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO


import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.ResizableTile
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen (fullscreenFull)
import XMonad.Layout.Cross(simpleCross)
import XMonad.Layout.Spiral(spiral)
import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.IndependentScreens
import XMonad.Layout.Minimize
import XMonad.Layout.CenteredMaster(centerMaster)

import Graphics.X11.ExtraTypes.XF86
import qualified System.IO
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified Data.ByteString as B

import Control.Monad (liftM2)

--mod4Mask    = super key
--mod1Mask    = alt key
--controlMask = ctrl key
--shiftMask   = shift key

myModMask                     = mod4Mask
myTerminal = "kitty"
mydefaults = def {
          normalBorderColor   = "#4c566a"
        , focusedBorderColor  = "#b5179e"
        , focusFollowsMouse   = True
        , mouseBindings       = myMouseBindings
        , workspaces          = myWorkspaces
        , keys                = myKeys
        , modMask             = myModMask
        , borderWidth         = 5
        , layoutHook          = myLayoutHook
        , startupHook         = myStartupHook
        , manageHook          = myManageHook
        , handleEventHook     = fullscreenEventHook <+> docksEventHook <+> minimizeEventHook
        , terminal            = myTerminal
        }

-- Autostart
myStartupHook = do
    spawn "$HOME/.xmonad/scripts/autostart.sh"
    setWMName "LG3D"

encodeCChar = map fromIntegral . B.unpack

myTitleColor = "#c91a1a" -- color of window title
myTitleLength = 80 -- truncate window title to this length
myCurrentWSColor = "#6790eb" -- color of active workspace
myVisibleWSColor = "#aaaaaa" -- color of inactive workspace
myUrgentWSColor = "#c91a1a" -- color of workspace with 'urgent' window
myHiddenNoWindowsWSColor = "white"

myLayoutHook = spacingRaw True (Border 0 5 5 5) True (Border 5 5 5 5) True $ gaps [(U,35), (D,5), (R,5), (L,5)]
               $ avoidStruts
               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT)
               $ smartBorders
               $ tiled ||| Grid ||| spiral (6/7) ||| ThreeColMid 1 (3/100) (1/2) ||| noBorders Full
                    where
                    tiled   = Tall nmaster delta ratio
                    nmaster = 1
                    delta   = 3/100
                    ratio   = 1/2



--WORKSPACES
xmobarEscape = concatMap doubleLts
    where doubleLts '<' = "<<"
          doubleLts x = [x]

myWorkspaces :: [String]
myWorkspaces = clickable . (map xmobarEscape) $ ["\61612","\61899","\61947","\61635","\61502","\61501","\61705","\61564","\62150","\61872"]
    where
               clickable l = [ "<action=xdotool key super+" ++ show (n) ++ ">" ++ ws ++ "</action>" | (i,ws) <- zip [1, 2, 3, 4, 5, 6, 7, 8, 9, 0] l, let n = i ]

-- window manipulations
myManageHook = composeAll . concat $
    [ [isDialog --> doCenterFloat ]
    , [isFullscreen --> doFullFloat ]
    , [className =? c --> doCenterFloat | c <- myCFloats]
    , [title =? t --> doFloat | t <- myTFloats]
    , [resource =? r --> doFloat | r <- myRFloats]
    , [resource =? i --> doIgnore | i <- myIgnores]
    --, [className =? c --> doShift (myWorkspaces !! 0) <+> viewShift (myWorkspaces !! 0)        | c <- my1Shifts]
--    , [className =? c --> doShift (myWorkspaces !! 1) <+> viewShift (myWorkspaces !! 1)        | c <- my2Shifts]
--    , [className =? c --> doShift (myWorkspaces !! 2) <+> viewShift (myWorkspaces !! 2)        | c <- my3Shifts]
--    , [className =? c --> doShift (myWorkspaces !! 3) <+> viewShift (myWorkspaces !! 3)        | c <- my4Shifts]
--    , [className =? c --> doShift (myWorkspaces !! 4) <+> viewShift (myWorkspaces !! 4)        | c <- my5Shifts]
--    , [className =? c --> doShift (myWorkspaces !! 5) <+> viewShift (myWorkspaces !! 5)        | c <- my6Shifts]
--    , [className =? c --> doShift (myWorkspaces !! 6) <+> viewShift (myWorkspaces !! 6)        | c <- my7Shifts]
--    , [className =? c --> doShift (myWorkspaces !! 7) <+> viewShift (myWorkspaces !! 7)        | c <- my8Shifts]
--    , [className =? c --> doShift (myWorkspaces !! 8) <+> viewShift (myWorkspaces !! 8)        | c <- my9Shifts]
--    , [className =? c --> doShift (myWorkspaces !! 9) <+> viewShift (myWorkspaces !! 9)        | c <- my10Shifts]
       ]
    where
--    viewShift    = doF . liftM2 (.) W.greedyView W.shift
    myCFloats = ["Arandr", "Arcolinux-calamares-tool.py", "Arcolinux-tweak-tool.py", "Arcolinux-welcome-app.py", "Galculator", "feh", "mpv"]
    myTFloats = ["Downloads", "Save As..."]
    myRFloats = []
    myIgnores = ["desktop_window"]
    my1Shifts = ["Brave"]
    -- my2Shifts = []
    -- my3Shifts = ["Inkscape"]
    -- my4Shifts = []
    -- my5Shifts = ["Gimp", "feh"]
    -- my6Shifts = ["vlc", "mpv"]
    -- my7Shifts = ["Virtualbox"]
    -- my8Shifts = ["Thunar"]
    -- my9Shifts = []
    -- my10Shifts = ["discord"]

-- keys config

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  ----------------------------------------------------------------------
  -- SUPER + FUNCTION KEYS

  [ ((modMask, xK_c), spawn $ "conky-toggle" )
  , ((modMask, xK_q), kill )
  , ((modMask, xK_v), spawn $ "pavucontrol" )
  , ((modMask, xK_Escape), spawn $ "xkill"  )
  , ((modMask, xK_Return), spawn $ myTerminal)
  , ((modMask, xK_F1), spawn $ "brave")
  , ((modMask, xK_F2), spawn $ "$HOME/.local/share/JetBrains/android-studio/bin/studio.sh")
  , ((modMask, xK_F3), spawn $ "$HOME/.local/share/JetBrains/Rider-2021.2.1/bin/rider.sh")
  , ((modMask, xK_p), spawn $ "rofi -show-icons -show run" )

  -- SUPER + SHIFT KEYS

  , ((modMask .|. shiftMask , xK_Return ), spawn $ "nemo")
  , ((modMask .|. shiftMask , xK_r ), spawn $ "xmonad --recompile && xmonad --restart")
  , ((modMask .|. shiftMask , xK_s ), spawn $ "flameshot gui")
  , ((modMask .|. shiftMask , xK_x ), spawn $ "arcolinux-logout" )

  -- CONTROL + ALT KEYS

  , ((controlMask .|. mod1Mask , xK_Next ), spawn $ "conky-rotate -n")
  , ((controlMask .|. mod1Mask , xK_Prior ), spawn $ "conky-rotate -p")
  , ((controlMask .|. mod1Mask , xK_o ), spawn $ "$HOME/.xmonad/scripts/picom-toggle.sh")
  , ((controlMask .|. mod1Mask , xK_u ), spawn $ "pavucontrol")
  , ((controlMask .|. mod1Mask , xK_Return ), spawn $ myTerminal)

  -- ALT + ... KEYS

  --, ((mod1Mask, xK_r), spawn $ "xmonad --recompile && xmonad --restart" )

  --SCREENSHOTS

  , ((0, xK_Print), spawn $ "scrot 'ArcoLinux-%Y-%m-%d-%s_screenshot_$wx$h.jpg' -e 'mv $f $$(xdg-user-dir PICTURES)'")
  , ((controlMask, xK_Print), spawn $ "xfce4-screenshooter" )
  , ((controlMask .|. shiftMask , xK_Print ), spawn $ "gnome-screenshot -i")


  --MULTIMEDIA KEYS

  -- Mute volume
  , ((0, xF86XK_AudioMute), spawn $ "amixer -q set Master toggle")

  -- Decrease volume
  , ((0, xF86XK_AudioLowerVolume), spawn $ "amixer -q set Master 5%-")

  -- Increase volume
  , ((0, xF86XK_AudioRaiseVolume), spawn $ "amixer -q set Master 5%+")

  -- Increase brightness
  , ((0, xF86XK_MonBrightnessUp),  spawn $ "xbacklight -inc 5")

  -- Decrease brightness
  , ((0, xF86XK_MonBrightnessDown), spawn $ "xbacklight -dec 5")

  , ((0, xF86XK_AudioPlay), spawn $ "playerctl play-pause")
  , ((0, xF86XK_AudioNext), spawn $ "playerctl next")
  , ((0, xF86XK_AudioPrev), spawn $ "playerctl previous")
  , ((0, xF86XK_AudioStop), spawn $ "playerctl stop")


  --------------------------------------------------------------------
  --  XMONAD LAYOUT KEYS

  -- Cycle through the available layout algorithms.
  , ((modMask, xK_space), sendMessage NextLayout)

  --Focus selected desktop
  --, ((mod1Mask, xK_Tab), nextWS)

  --Focus selected desktop
 -- , ((modMask, xK_Tab), nextWS)

  --Focus selected desktop
  --, ((controlMask .|. mod1Mask , xK_Left ), prevWS)

  --Focus selected desktop
  --, ((controlMask .|. mod1Mask , xK_Right ), nextWS)

  --  Reset the layouts on the current workspace to default.
  , ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)

  -- Move focus to the next window.
  , ((modMask, xK_j), windows W.focusDown)

  -- Move focus to the previous window.
  , ((modMask, xK_k), windows W.focusUp  )

  -- Move focus to the master window.
  , ((modMask .|. shiftMask, xK_m), windows W.focusMaster  )

  -- Swap the focused window with the next window.
  , ((modMask .|. shiftMask, xK_j), windows W.swapDown  )

  -- Swap the focused window with the next window.
  , ((controlMask .|. modMask, xK_Down), windows W.swapDown  )

  -- Swap the focused window with the previous window.
  , ((modMask .|. shiftMask, xK_k), windows W.swapUp    )

  -- Swap the focused window with the previous window.
  , ((controlMask .|. modMask, xK_Up), windows W.swapUp  )

  -- Shrink the master area.
  , ((modMask .|. shiftMask , xK_h), sendMessage Shrink)

  -- Expand the master area.
  , ((modMask .|. shiftMask , xK_l), sendMessage Expand)

  -- Push window back into tiling.
  , ((modMask .|. shiftMask , xK_t), withFocused $ windows . W.sink)

  -- Increment the number of windows in the master area.
  , ((modMask .|. modMask, xK_Left), sendMessage (IncMasterN 1))

  -- Decrement the number of windows in the master area.
  , ((modMask .|. modMask, xK_Right), sendMessage (IncMasterN (-1)))

  ]
    ++
  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [((m .|. modMask, k), windows $ f i)

  --Keyboard layouts
  --qwerty users use this line
   | (i, k) <- zip (XMonad.workspaces conf) [xK_1,xK_2,xK_3,xK_4,xK_5,xK_6,xK_7,xK_8,xK_9,xK_0]
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)
      , (\i -> W.greedyView i . W.shift i, shiftMask)] ]
   ++
  -- ctrl-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- ctrl-shift-{w,e,r}, Move client to screen 1, 2, or 3
   [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_w, xK_e] [1, 0]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, 1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, 2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, 3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))

    ]

--XMOBAR
xmobarPath :: String
xmobarPath = "$HOME/.cabal/bin/"
main = do
        xmproc0 <- spawnPipe $ xmobarPath ++ "xmobar -x 0 $HOME/.config/.xmobarrc" -- xmobar monitor 1
        xmproc1 <- spawnPipe $ xmobarPath ++ "xmobar -x 1 $HOME/.config/.xmobarrc" -- xmobar monitor 2
        xmonad $ ewmh $ mydefaults {
        logHook =  dynamicLogWithPP $ def {
        ppOutput = \x -> System.IO.hPutStrLn xmproc0 x  >> System.IO.hPutStrLn xmproc1 x
        , ppTitle = xmobarColor myTitleColor "" . ( \ str -> "")
        , ppCurrent = xmobarColor myCurrentWSColor "" . wrap """"
        , ppVisible = xmobarColor myVisibleWSColor "" . wrap """"
        , ppHidden = wrap """"
        , ppHiddenNoWindows = xmobarColor myHiddenNoWindowsWSColor ""
        , ppUrgent = xmobarColor myUrgentWSColor ""
        , ppSep = "  "
        , ppWsSep = "  "
        , ppLayout = (\ x -> case x of
           "Spacing Tall"                 -> "<fn=1>Tall</fn>"
           "Spacing Grid"                 -> "<fn=1>Grid</fn>"
           "Spacing Spiral"               -> "<fn=1>Spiral</fn>"
           "Spacing ThreeCol"             -> "<fn=1>ThreeColMid</fn>"
           "Spacing Full"                 -> "<fn=1>Full</fn>"
           _                              -> x )
 }
}
