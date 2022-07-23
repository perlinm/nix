{-- todo:

floating window management - get raise on click to work properly
sort out border color bug(?)
sort/comment imported modules - i.e. what is each module used for?

--}

import Control.Monad
import Data.Map as M
import Data.Monoid
import System.Exit
import System.IO
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.ConstrainedResize as CR
import XMonad.Actions.CopyWindow
import XMonad.Actions.Navigation2D
import XMonad.Actions.WorkspaceNames
import XMonad.Config.Xfce
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Hooks.SetWMName
import XMonad.Layout.Fullscreen as FS
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.WindowNavigation
import XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.NamedScratchpad

---------------------------------------------------------------------------------
-- variables

myBorderWidth = 1
myNormalBorderColor = "#000000"
myFocusedBorderColor = "#777777"
myFocusFollowsMouse = True

myWorkspaces = Prelude.map show [0..10] ++ ["NSP"]

scriptDir = "~/scripts/"

myTerminal = "xfce4-terminal"
myRun = "bashrun"
myLock = "xtrlock"

---------------------------------------------------------------------------------
-- window rules

myManageHook = composeOne . concat $
  [
    [ (roleName =? r) -?> doSink | r <- sinkRoles ],
    [ (title =? t) -?> doCenterFloat | t <- floatTitles ],
    [ (className =? c) -?> doCenterFloat | c <- floatClasses ],
    [ (className =? c) -?> doIgnore | c <- ignoreClasses ]
  ]
  where
    roleName = stringProperty "WM_WINDOW_ROLE"
    doSink = (ask >>= doF . W.sink) <+> doF W.swapUp
    sinkRoles = [ "app", "conversation" ]
    floatTitles = [ "bashrun" ]
    floatClasses = [ "Xfce4-taskmanager", "Xfce4-power-manager-settings",
                     "Xfce4-appfinder", "Xfce4-panel",
                     "Nm-connection-editor", "Nm-openconnect-auth-dialog",
                     "", " ", "Wicd-client.py", "Python2",
                     "matplotlib", "Matplotlib",
                     "Thunar", "Arandr", "Wrapper-1.0" ]
    ignoreClasses = [ "Xfce4-notifyd" ]

---------------------------------------------------------------------------------
-- scratchpads

termName = "term"
calcName = "calc"
altTermName = "alt-term"
htopName = "htop"
mixerName = "mixer"
myScratchPads = [ padTemplate termName termHook,
                  padTemplate altTermName altTermHook,
                  padTemplate calcName calcHook,
                  padTemplate htopName htopHook,
                  NS mixerName mixerCommand mixerID mixerHook ]
  where
    padTemplate name padHook =
        NS name (scriptDir ++ "pads.zsh " ++ name) (title =? ("pad-" ++ name)) padHook
    termHook = customFloating $ W.RationalRect l t w h
      where
        h = 1/2
        w = 0.45
        t = (1-h)*9/10
        l = (1/2-w)/2
    altTermHook = customFloating $ W.RationalRect l t w h
      where
        h = 1/2
        w = 0.45
        t = (1-h)*9/10
        l = 1/2+(1/2-w)/2
    calcHook = customFloating $ W.RationalRect l t w h
      where
        h = 2/3
        w = 2/5
        t = 1/25
        l = 1-w
    htopHook = customFloating $ W.RationalRect l t w h
      where
        h = 4/5
        w = 1/2
        t = (1-h)/2
        l = (1-w)/2
    mixerCommand = "pavucontrol"
    mixerID = className =? "Pavucontrol"
    mixerHook = customFloating $ W.RationalRect l t w h
      where
        h = 4/5
        w = 1/2
        t = (1-h)/2
        l = (1-w)/2

---------------------------------------------------------------------------------
-- layout definitions

normal = renamed [Replace "normal"] $ ResizableTall 1 (1/100) (1/2) []
code = renamed [Replace "code"] $ ResizableTall 1 (1/100) (51/100) []
latex = renamed [Replace "latex"] $ ResizableTall 1 (1/100) (37/100) []
full = renamed [Replace "full"] $ Full
grid = renamed [Replace "grid" ] $ ThreeCol 1 (1/100) (1/3)

(|+|) :: (LayoutClass l a, LayoutClass r a) =>
         (l a, Int) -> r a -> (Choose l r a, Int)
(a,n) |+| b = (a ||| b, n+1)
countedLayouts = (normal,1) |+| code |+| latex |+| grid
layouts = fst countedLayouts
layoutCount = snd countedLayouts

myLayoutHook = smartBorders $ avoidStruts $ windowNavigation $ smartSpacing 3 $
               toggleLayouts full layouts

myPlacement = withGaps (16,16,16,16) (fixed (0.5,0.5))

---------------------------------------------------------------------------------
-- key commands

-- define number row, arrow keys, and modification keys
numRow = ["`"] ++ (Prelude.map show [1..9]) ++ ["0","-"]
arrows = [["n","i","u","e"],["r","s","p","v"],["<L>","<R>","<U>","<D>"]]
modKeys = ["","C-","S-","M1-"]

-- define window actions in each direction performed by modification keys
actLeft = [prevWS, swapTo Prev, shiftToPrev, shiftToPrev >> prevWS]
actRight = [nextWS, swapTo Next, shiftToNext, shiftToNext >> nextWS]
actUp = [prevScreen, swapPrevScreen, shiftPrevScreen, shiftPrevScreen >> swapPrevScreen]
actDn = [nextScreen, swapNextScreen, shiftNextScreen, shiftNextScreen >> swapNextScreen]

{-
  for each set of actions, pair up modification keys with the respective action
  e.g. ("C-", swapTo Prev)
-}
modActions = [ zip modKeys dirFuns | dirFuns <- [actLeft,actRight,actUp,actDn] ]


-- for each set of arrow keys, pair up arrows with respective modification key and action
dirControlsSet = join [ zip dirs modActions | dirs <- arrows ]

-- collect all appropriate combinations of keys and workspace actions into a single list
dirControls = join [ [ (dir,mod,fun) | let dir = fst set, (mod,fun) <- snd set ]
                     | set <- dirControlsSet ]

-- note: check output of 'xmodmap' for modifier key map (i.e. M1, M4, etc.)

myKeys = \conf -> mkKeymap conf $
    ---------- workspace management ----------
    [
     ("M4-" ++ mod ++ key, windows $ func ws)
         | (ws,key) <- zip myWorkspaces numRow,
           (mod,func) <- [ ("", W.greedyView), ("S-", W.shift),
                           ("M1-", \i -> W.greedyView i . W.shift i) ]
    ]
    ++
    [ ("M4-C-" ++ key, swapWithCurrent ws) | (ws,key) <- zip myWorkspaces numRow ]
    ++
    [ ("M4-" ++ mod ++ dir, func) | (dir,mod,func) <- dirControls ]
    ++
    [
     ("M1-z", toggleWS' ["NSP"]),
     ---------- layout management ----------
     ("M4-<Space>", nextLayout),
     ("M4-S-<Space>", prevLayout),
     ("M4-M1-<Space>", setLayout $ layoutHook conf),
     ("M4-z", sendMessage $ Toggle "full"),
     ("M4-x", sendMessage Shrink),
     ("M4-c", sendMessage Expand),
     ("M4-S-x", sendMessage $ IncMasterN 1),
     ("M4-S-c", sendMessage $ IncMasterN (-1)),
     ("M4-M1-x", sendMessage MirrorExpand),
     ("M4-M1-c", sendMessage MirrorShrink),
     ("M4-b", sendMessage ToggleStruts),
     ---------- window management ----------
     -- ("M1-<Tab>", windows focusUp >> windows shiftMaster),
     -- ("M1-S-<Tab>", windows focusDown >> windows shiftMaster),
     ("M4-w", windows focusUp),
     ("M4-f", windows focusDown),
     ("M4-S-w", windows swapUp),
     ("M4-S-f", windows swapDown),
     ("M4-q", windows focusMaster),
     ("M4-a", windows shiftMaster),
     ("M4-t", withFocused $ windows . sink),
     ("M4-<Esc>", kill),
     ---------- spawning ----------
     ("M4-<Tab>", spawn $ terminal conf),
     ("M1-`", spawn myRun),
     ("M1-C-`", spawn "xfce4-appfinder"),
     ("M1-<F1>", namedScratchpadAction myScratchPads termName),
     ("M1-<F2>", namedScratchpadAction myScratchPads altTermName),
     ("M1-<F3>", namedScratchpadAction myScratchPads calcName),
     ("M1-<F4>", namedScratchpadAction myScratchPads htopName),
     ("M1-<F5>", namedScratchpadAction myScratchPads mixerName),
     ---------- media keys ----------
     ("<XF86AudioMute>", runScript "volume.sh toggle"),
     ("<XF86AudioLowerVolume>", runScript "volume.sh dec"),
     ("<XF86AudioRaiseVolume>", runScript "volume.sh inc"),
     ("S-<XF86AudioMute>", runScript "volume.sh mic toggle"),
     ("S-<XF86AudioLowerVolume>", runScript "volume.sh mic dec"),
     ("S-<XF86AudioRaiseVolume>", runScript "volume.sh mic inc"),
     ("<XF86MonBrightnessDown>", runScript "light.sh dec"),
     ("<XF86MonBrightnessUp>", runScript "light.sh inc"),
     ("<XF86TouchpadToggle>", runScript "touchpad-toggle.sh"),
     ---------- testing media keys (do not currently work) ----------
     ("M4-<F8>", spawn "arandr"),
     -- ("<XF86Display>", spawn "arandr"),
     -- ("<XF86Search>", spawn "xfce4-appfinder"),
     ---------- volume ----------
     ("C-/", runScript "volume.sh toggle"),
     ("C-<D>", runScript "volume.sh dec"),
     ("C-<U>", runScript "volume.sh inc"),
     ("C-S-<D>", runScript "volume.sh min"),
     ("C-S-<U>", runScript "volume.sh max"),
     ("C-S-/", runScript "volume.sh med"),
     ---------- backlights ----------
     ("M1-C-<U>", runScript "light.sh max"),
     ("M1-C-<D>", runScript "light.sh dim"),
     ("M1-C-/", runScript "light.sh med"),
     ("M1-S-<U>", runScript "kbd_light.sh up"),
     ("M1-S-<D>", runScript "kbd_light.sh off"),
     ("M1-S-/", runScript "kbd_light.sh 1"),
     ---------- screenshots ----------
     ("M4-\\", runScript "print-screen.sh"),
     ("M4-S-\\", runScript "print-screen.sh -s"),
     ---------- misc ----------
     ("C-q", runScript "null.sh"),
     ("C-<Esc>", spawn myLock),
     ("M4-k", runScript "layout-toggle.sh"),
     ("M4-/", windows copyToAll),
     ("M4-S-/", killAllOtherCopies),
     ("C-M4-<Backspace>", io $ exitWith ExitSuccess)
    ]
    where nextLayout = sendMessage NextLayout
          prevLayout = do { replicateM_ (layoutCount-1) $ sendMessage NextLayout}
          runScript s = spawn $ scriptDir ++ s
          runSudoScript s = spawn $ "sudo " ++ scriptDir ++ s

---------------------------------------------------------------------------------
-- cursor actions

myMouseBindings (XConfig {}) = fromList $
  [
   ((mod4Mask, button1), move),
   ((mod4Mask, button3), resize False),
   ((mod4Mask .|. shiftMask, button3), resize True)
  ]
    where
      move = raiseFloating mouseMoveWindow
      resize b = raiseFloating $ \w -> CR.mouseResizeWindow w b
      raiseFloating f = \w -> withWindowSet $ \s -> do
                          XMonad.focus w >> (if M.member w $ floating s
                                             then f w >> windows W.shiftMaster
                                             else f w)

---------------------------------------------------------------------------------
-- bring floating window to the front when clicked
-- note: only works on unfocused windows (i.e. it does not work properly)

floatClickFocusHandler :: Event -> X All
floatClickFocusHandler ButtonEvent { ev_window = w, ev_event_type = t }
    | t == buttonPress = do withWindowSet $ \ws -> do
                              if M.member w $ floating ws
                              then (XMonad.focus w >> windows shiftMaster)
                              else return ()
                            return $ All True
floatClickFocusHandler _ = return $ All True

---------------------------------------------------------------------------------
-- run XMonad

main = do
  xmonad $ withNavigation2DConfig def $ ewmhFullscreen . ewmh $ docks $ xfceConfig {
        terminal = myTerminal,
        borderWidth = myBorderWidth,
        normalBorderColor = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        focusFollowsMouse = myFocusFollowsMouse,
        modMask = mod4Mask,
        XMonad.keys = myKeys,
        XMonad.workspaces = myWorkspaces,
        layoutHook = myLayoutHook,
        manageHook = namedScratchpadManageHook myScratchPads
                     <+> placeHook myPlacement
                     <+> myManageHook
                     <+> manageDocks
                     <+> fullscreenManageHook
                     <+> manageHook def,
        mouseBindings = myMouseBindings,
        startupHook = setWMName "LG3D",
        handleEventHook = FS.fullscreenEventHook
                          <+> floatClickFocusHandler
    }
