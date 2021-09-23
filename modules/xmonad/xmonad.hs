{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}
{-# OPTIONS_GHC -O2 #-}

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Applicative
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Graphics.X11.ExtraTypes.XF86
import System.Exit (exitSuccess)
import System.Posix.Signals (signalProcess)
import System.Posix.Types (ProcessID)
import System.Taffybar.Support.PagerHints (pagerHints)
import XMonad hiding ((|||))
import XMonad.Actions.CopyWindow (copy, copyToAll, killAllOtherCopies)
import XMonad.Actions.GridSelect (gridselect, gridselectWorkspace, GSConfig(..), HasColorizer)
import XMonad.Actions.Minimize (maximizeWindow, minimizeWindow, withLastMinimized)
import XMonad.Actions.PhysicalScreens (getScreen, horizontalScreenOrderer, PhysicalScreen(..))
import XMonad.Actions.Plane (planeKeys, Limits(..), Lines(..))
import XMonad.Actions.SwapWorkspaces (swapWithCurrent)
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Actions.WindowMenu (windowMenu)
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.InsertPosition (insertPosition, Focus(..), Position(..))
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, docksStartupHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (doFullFloat, isDialog, isFullscreen, pid)
import XMonad.Hooks.Minimize (minimizeEventHook)
import XMonad.Hooks.UrgencyHook (focusUrgent, withUrgencyHook, NoUrgencyHook(..))
import XMonad.Layout.BoringWindows (boringWindows, focusDown, focusMaster, focusUp)
import XMonad.Layout.Decoration (shrinkText, Theme(..))
import XMonad.Layout.DwmStyle (dwmStyle)
import XMonad.Layout.Grid (Grid(..))
import XMonad.Layout.LayoutCombinators ((|||), JumpToLayout(..))
import XMonad.Layout.Maximize (maximize)
import XMonad.Layout.Minimize (minimize)
import XMonad.Layout.Named (named)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Layout.Spacing (spacingRaw, Border(..))
import XMonad.Layout.TrackFloating (trackFloating)
import XMonad.Prompt (XPConfig(..), XPPosition(..))
import XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import XMonad.Prompt.FuzzyMatch (fuzzyMatch, fuzzySort)
import qualified XMonad.StackSet as SS
import XMonad.Util.Compton (invert)
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run (runProcessWithInput)

myTerminal = "alacritty"

myFocusFollowsMouse = True

myClickJustFocuses = False

myBorderWidth = 0

myModMask = mod4Mask

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

myNormalBorderColor = "gray"

myFocusedBorderColor = "black"

myFont size = "xft:DejaVu Sans:size=" ++ show size

myXPConfig = def
  { position = Top
  , height = 25
  , bgColor = "black"
  , promptBorderWidth = 0
  , searchPredicate = fuzzyMatch
  , sorter = fuzzySort
  , alwaysHighlight = True
  , font = myFont 11
  }

withPid :: Window -> (ProcessID -> X ()) -> X ()
withPid w f = do
  maybePid <- runProcessWithInput "xcb-client-id" [show w] ""
  maybePid <- if null maybePid
              then runQuery pid w
              else return $ Just $ read maybePid
  whenJust maybePid f

kill9Window :: Window -> X ()
kill9Window w = withWindowSet $ \ws ->
  when (SS.member w ws) $ withPid w (io . signalProcess 9)

kill9 :: X ()
kill9 = withFocused kill9Window

killSafe :: X ()
killSafe = withFocused killWindowSafe

killWindowSafe :: Window -> X ()
killWindowSafe w = withWindowSet $ \ws ->
  when (SS.member w ws) $ killWindow w

decorateName' :: Window -> X String
decorateName' w = show <$> getName w

goToSelectedOnWorkspace gsConfig = do
  let keyValuePair w = (, w) `fmap` decorateName' w
  wins <- gets (SS.index . windowset)
  when (length wins > 1) $ do
    namedWindows <- mapM keyValuePair wins
    maybeWindow <- gridselect gsConfig namedWindows
    case maybeWindow of
      Just window -> windows $ SS.focusWindow window
      Nothing     -> return ()

myKeys conf@XConfig { XMonad.modMask = modm } = M.union (planeKeys modm (Lines 3) Linear) $ M.fromList $
  -- Launch terminal.
  [ ((modm, xK_r), spawn $ XMonad.terminal conf)
  -- Launch terminal.
  , ((modm .|. shiftMask, xK_r), spawn "xterm")
  -- Launch mc.
  , ((modm, xK_e), spawn "alacritty -e mc -x")
  -- Launch application.
  , ((modm, xK_F2), shellPrompt myXPConfig)
  -- Close the focused window.
  , ((modm, xK_c), killSafe)
  -- Kill the focused window.
  , ((modm .|. shiftMask, xK_c), kill9)
  -- Rotate through the available layout algorithms.
  , ((modm, xK_space), sendMessage NextLayout)
  -- Reset the layouts on the current workspace to default.
  , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
  -- Resize viewed windows to the correct size.
  , ((modm, xK_n), refresh)
  -- Move focus to the next window.
  , ((modm, xK_Tab), focusDown)
  -- Move focus to the previous window.
  , ((modm .|. shiftMask, xK_Tab), focusUp)
  -- Move focus to the master window.
  , ((modm, xK_Return), focusMaster)
  -- Swap the focused window and the master window.
  , ((modm .|. shiftMask, xK_Return), windows SS.swapMaster)
  -- Swap the focused window with the previous window.
  , ((modm .|. controlMask, xK_comma), windows SS.swapUp)
  -- Swap the focused window with the next window.
  , ((modm .|. controlMask, xK_period), windows SS.swapDown)
  -- Shrink the master area.
  , ((modm, xK_comma), sendMessage Shrink)
  -- Expand the master area.
  , ((modm, xK_period), sendMessage Expand)
  -- Push window back into tiling.
  , ((modm, xK_t), withFocused $ windows . SS.sink)
  -- Increment the number of windows in the master area.
  , ((modm .|. shiftMask, xK_comma), sendMessage (IncMasterN (-1)))
  -- Decrement the number of windows in the master area.
  , ((modm .|. shiftMask, xK_period), sendMessage (IncMasterN 1))
  -- Toggle the status bar gap.
  , ((modm, xK_b), sendMessage ToggleStruts)
  -- Select workspace.
  , ((modm, xK_z), gridselectWorkspace myGSConfig SS.greedyView)
  -- Select window on current workspace.
  , ((modm, xK_x), goToSelectedOnWorkspace myGSConfig)
  -- Quit xmonad.
  , ((modm .|. shiftMask, xK_q), confirmPrompt myXPConfig "exit" $ io exitSuccess)
  -- Restart xmonad.
  , ((modm, xK_q), restart "xmonad" True)
  -- Turn off screen.
  , ((modm, xK_F6), spawn "sleep 0.5; xset dpms force off")
  -- Focus urgent window (window with notification).
  , ((modm, xK_u), focusUrgent)
  -- Make focused window always visible.
  , ((modm, xK_v), windows copyToAll)
  -- Toggle window state back.
  , ((modm .|. shiftMask, xK_v), killAllOtherCopies)
  -- Increase monitor backlight level.
  , ((0, xF86XK_MonBrightnessUp), spawn "brightnessctl s 10%+")
  -- Decrease monitor backlight level.
  , ((0, xF86XK_MonBrightnessDown), spawn "brightnessctl s 10%-")
  -- Set monitor backlight level to maximum value.
  , ((shiftMask, xF86XK_MonBrightnessUp), spawn "brightnessctl s 100%")
  -- Set monitor backlight level to minimum value.
  , ((shiftMask, xF86XK_MonBrightnessDown), spawn "brightnessctl s 1%")
  -- Toggle microphone.
  , ((0, xF86XK_AudioMicMute), spawn "pactl set-source-mute @DEFAULT_SOURCE@ toggle")
  -- Toggle sound.
  , ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
  -- Raise volume.
  , ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
  -- Lower volume.
  , ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
  -- dbus-send --dest=com.github.chjj.compton._0 / com.github.chjj.compton.win_set uint32:0x5a0000a string:invert_color_force uint16:1
  , ((modm, xK_i), withDisplay $ \dpy -> withFocused $ io . invert dpy)
  -- Invert colors.
  , ((modm .|. shiftMask, xK_i), spawn "xcalib -i -a")
  -- Clear invertions.
  , ((modm .|. controlMask .|. shiftMask, xK_i), spawn "xcalib -c")
  -- Open window menu.
  , ((modm, xK_o), windowMenu)
  -- Minimize focused window.
  , ((modm, xK_m), withFocused minimizeWindow)
  , ((modm .|. shiftMask, xK_m), withLastMinimized maximizeWindow)
  , ((modm, xK_w), withDisplay $ \dpy -> withFocused $ io . raiseWindow dpy)
  , ((modm, xK_f), sendMessage $ JumpToLayout "Full")
  , ((modm, xK_g), sendMessage $ JumpToLayout "Grid")
  , ((modm, xK_h), sendMessage $ JumpToLayout "Tiled")
  , ((modm, xK_j), sendMessage $ JumpToLayout "Mirror")
  , ((modm, xK_Escape), spawn "xscreensaver-command -lock")
  , ((0, xK_Print), spawn "maim ~/screenshot-$(date +%F-%T).png")
  , ((shiftMask, xK_Print), spawn "maim -i $(xdotool getactivewindow) ~/screenshot-$(date +%F-%T).png")
  , ((controlMask, xK_Print), spawn "maim -s -c 1,0,0,0.6 -p 10 ~/screenshot-$(date +%F-%T).png")
  ]
  ++
  [ ((m .|. modm, k), windows $ f i)
  | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
  , (f, m) <- -- mod-[1..9], Switch to workspace N.
              [ (SS.greedyView, 0)
              -- mod-shift-[1..9], Move client to workspace N.
              , (SS.shift, shiftMask)
              -- mod-ctrl-[1..9], Copy client to workspace N.
              , (copy, controlMask)
              -- mod-ctrl-shift-[1..9], Swap workspace with workspace N.
              , (swapWithCurrent, controlMask .|. shiftMask)
              ]
  ]
  ++
  [ ((m .|. modm, key), void $ runMaybeT $ (MaybeT . getScreen horizontalScreenOrderer) sc >>= MaybeT . screenWorkspace >>= lift . windows . f)
  | (key, sc) <- zip [xK_a, xK_s, xK_d] [0..]
  , (f, m) <- -- mod-{a,s,d}, Switch to physical/Xinerama screens 1, 2, or 3.
              [ (SS.view, 0)
              -- mod-shift-{a,s,d}, Move client to screen 1, 2, or 3.
              , (SS.shift, shiftMask)
              ]
  ]

myMouseBindings XConfig { XMonad.modMask = modm } = M.fromList
  -- Set the window to floating mode and move by dragging.
  [ ((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows SS.shiftMaster)
  -- Close window.
  , ((modm, button2), killWindowSafe)
  -- Kill window.
  , ((modm .|. shiftMask, button2), kill9Window)
  -- Set the window to floating mode and resize by dragging.
  , ((modm, button3), \w -> focus w >> mouseResizeWindow w >> windows SS.shiftMaster)
  -- Close window.
  , ((0, 8), killWindowSafe)
  -- Open window menu.
  , ((0, 9), \w -> windows (SS.focusWindow w) >> windowMenu)
  ]

myTheme = def { activeColor = "#000000"
              , inactiveColor = "#000000"
              , urgentColor = "#000000"
              , activeBorderColor = "#444444"
              , inactiveBorderColor = "#444444"
              , urgentBorderColor = "#444444"
              , activeTextColor = "#00FF00"
              , inactiveTextColor = "#FFFFFF"
              , urgentTextColor = "#FF0000"
              , fontName = myFont 11
              , decoHeight = 25
              }

myLayout = fullLayoutModifiers fullLayout |||
           tiledLayoutModifiers tiledLayout |||
           mirrorLayoutModifiers mirrorLayout |||
           gridLayoutModifiers gridLayout where
  fullLayoutModifiers = named "Full" . smartBorders . avoidStruts . maximize . minimize . boringWindows . trackFloating
  tiledLayoutModifiers = named "Tiled" . dwmStyle shrinkText myTheme . smartBorders . spacing . avoidStruts . maximize . minimize . boringWindows
  mirrorLayoutModifiers = named "Mirror" . dwmStyle shrinkText myTheme . smartBorders . spacing . avoidStruts . maximize . minimize . boringWindows
  gridLayoutModifiers = named "Grid" . dwmStyle shrinkText myTheme . smartBorders . spacing . avoidStruts . maximize . minimize . boringWindows
  spacing = spacingRaw True (Border 0 0 0 0) False (Border 2 2 2 2) True
  fullLayout = Full
  tiledLayout = Tall nmaster delta ratio
  mirrorLayout = Mirror tiledLayout
  gridLayout = Grid
  nmaster = 1 -- The default number of windows in the master pane.
  ratio = 1 / 2 -- Default proportion of screen occupied by master pane.
  delta = 3 / 100 -- Percent of screen to increment by when resizing panes.

-- To find the property name associated with a program, use > xprop | grep WM_CLASS.
myManageHook = manageDocks <> (isFullscreen --> doFullFloat) <> (masterCondition --> insertPosition Master Newer)
  where masterCondition = fmap (/= "Full") layoutName <&&> fmap not isDialog
        layoutName = liftX $ withWindowSet $ return . description . SS.layout . SS.workspace . SS.current

myEventHook e = minimizeEventHook e <> fullscreenEventHook e <> docksEventHook e

myLogHook = updatePointer (0.5, 0.5) (0, 0)

myGSConfig :: HasColorizer a => GSConfig a
myGSConfig = def { gs_cellheight = 200
                 , gs_cellwidth = 400
                 , gs_font = myFont 22
                 }

addNETSupported :: Atom -> X ()
addNETSupported x = withDisplay $ \dpy -> do
  r <- asks theRoot
  a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
  a <- getAtom "ATOM"
  liftIO $ do
    sup <- join . maybeToList <$> getWindowProperty32 dpy a_NET_SUPPORTED r
    when (fromIntegral x `notElem` sup) $
      changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]

addEWMHFullscreen :: X ()
addEWMHFullscreen = do
  wms <- getAtom "_NET_WM_STATE"
  wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
  mapM_ addNETSupported [wms, wfs]

myStartupHook = do
  addEWMHFullscreen
  docksStartupHook
  spawn "setxkbmap -model pc101 -layout us,ru -option grp:caps_toggle -option grp:switch -option grp_led:caps -option lv3:ralt_switch"
  spawn "sleep 1; xmodmap ~/.Xmodmap"
  spawn "feh --no-fehbg --bg-fill ~/Images/pic-3909-1920x1200.jpg"

myConfig = withUrgencyHook NoUrgencyHook $ ewmh $ pagerHints def
  { terminal           = myTerminal
  , focusFollowsMouse  = myFocusFollowsMouse
  , clickJustFocuses   = myClickJustFocuses
  , borderWidth        = myBorderWidth
  , modMask            = myModMask
  , workspaces         = myWorkspaces
  , normalBorderColor  = myNormalBorderColor
  , focusedBorderColor = myFocusedBorderColor
  , keys               = myKeys
  , mouseBindings      = myMouseBindings
  , layoutHook         = myLayout
  , manageHook         = myManageHook
  , handleEventHook    = myEventHook
  , logHook            = myLogHook
  , startupHook        = myStartupHook
  }

main :: IO ()
main = launch myConfig
