import Control.Monad
import Data.Map
import System.IO
import System.Exit
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.CopyWindow
import XMonad.Actions.FlexibleResize as Flex
import XMonad.Actions.NoBorders
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

script = "/home/perlinm/scripts/"

myTerminal = "xfce4-terminal"
myRun = "gmrun"

---------------------------------------------------------------------------------
-- window rules

myManageHook = composeAll . concat $
  [
    [ (className =? f) --> doCenterFloat | f <- floats ],
    [ (className =? i) --> doIgnore | i <- ignores ],
    [ (roleName =? s) --> (ask >>= doF . W.sink) | s <- sinks ]
  ]
  where
    roleName = stringProperty "WM_WINDOW_ROLE"
    floats = ["Xfce4-appfinder","Xfce4-panel","Nm-connection-editor",
              "Nm-openconnect-auth-dialog"," ","Wicd-client.py","Python2",
              "Wrapper","Thunar","Arandr"]
    ignores = ["Xfce4-notifyd"]
    sinks = ["gimp-image-window"]

---------------------------------------------------------------------------------
-- scratchpads

termName = "term"
calcName = "calc"
wifiName = "network"
htopName = "htop"
mixerName = "mixer"
myScratchPads = [ NS termName spawnTerm findTerm manageTerm,
                  NS calcName spawnCalc findCalc manageCalc,
                  NS wifiName spawnWifi findWifi manageWifi,
                  NS htopName spawnHtop findHtop manageHtop,
                  NS mixerName spawnMixer findMixer manageMixer ]
  where
    spawnTerm = script ++ "pads " ++ termName
    findTerm = title =? ("pad-" ++ termName)
    manageTerm = customFloating $ W.RationalRect l t w h
      where
        h = 1/2
        w = 0.45
        t = (1-h)*9/10
        l = (1/2-w)/2
    spawnCalc = script ++ "pads " ++ calcName
    findCalc = title =? ("pad-" ++ calcName)
    manageCalc = customFloating $ W.RationalRect l t w h
      where
        h = 2/3
        w = 2/5
        t = 1/25
        l = 1-w
    spawnWifi = script ++ "pads " ++ wifiName
    findWifi = title =? ("pad-" ++ wifiName)
    manageWifi = customFloating $ W.RationalRect l t w h
      where
        h = 1/2
        w = 1/2
        t = (1-h)/2
        l = (1-w)/2
    spawnHtop = script ++ "pads " ++ htopName
    findHtop = title =? ("pad-" ++ htopName)
    manageHtop = customFloating $ W.RationalRect l t w h
      where
        h = 4/5
        w = 1/2
        t = (1-h)/2
        l = (1-w)/2
    spawnMixer = "pavucontrol"
    findMixer = className =? "Pavucontrol"
    manageMixer = customFloating $ W.RationalRect l t w h
      where
        h = 4/5
        w = 1/2
        t = (1-h)/2
        l = (1-w)/2

---------------------------------------------------------------------------------
-- layout definitions

normal = renamed [Replace "normal"] $ ResizableTall 1 (1/50) (1/2) []
emacs = renamed [Replace "emacs"] $ ResizableTall 1 (1/50) (39/100) []
chat = renamed [Replace "chat"] $ ResizableTall 1 (1/50) (73/100) []
skype = renamed [Replace "skype"] $ ResizableTall 1 (1/50) (64/100) []
full = renamed [Replace "full"] $ Full
myLayoutHook = smartBorders $ avoidStruts $ windowNavigation $
               toggleLayouts full ( normal ||| emacs ||| chat ||| skype )

myPlacement = withGaps (16,16,16,16) (fixed (0.5,0.5))

---------------------------------------------------------------------------------
-- key commands

-- define number row, direction keys, and modification keys
numRow = ["`"] ++ (Prelude.map show [1..9]) ++ ["0","-"]
arrows = [["<L>","<R>","<U>","<D>"],["n","i","u","e"]]
mod_keys = ["","C-","S-","M1-"]
-- define direction functions
leftFuns = [prevWS, swapTo Prev, shiftToPrev, shiftToPrev >> prevWS]
rightFuns = [nextWS, swapTo Next, shiftToNext, shiftToNext >> nextWS]
upFuns = [prevScreen, swapPrevScreen, shiftPrevScreen,
           shiftPrevScreen >> swapPrevScreen]
downFuns = [nextScreen, swapNextScreen, shiftNextScreen,
             shiftNextScreen >> swapNextScreen]
-- pair up modification keys with functions
funsCol = [ zip mod_keys dirFuns
                    | dirFuns <- [leftFuns,rightFuns,upFuns,downFuns] ]
-- pair up directions with appropriate functions
dirControlsCol = join [ zip dirs funsCol | dirs <- arrows ]
-- collect all combinations of directions, modification keys, and functions
--   into a single list
dirControls = join [[(fst c,mod,fun) | (mod,fun) <- snd c ]
                        | c <- dirControlsCol]

myKeys = \conf -> mkKeymap conf $
    ---------- workspace management ----------
    [
     ("M4-" ++ mod ++ key, windows $ func ws)
         | (ws,key) <- zip myWorkspaces numRow,
           (func,mod) <- [(W.greedyView, ""),(W.shift, "S-"),
                          (\i -> W.greedyView i . W.shift i,"M1-")]
    ]
    ++
    [
     ("M4-C-" ++ key, swapWithCurrent ws) | (ws,key) <- zip myWorkspaces numRow
    ]
    ++
    [
     ("M4-" ++ mod ++ dir, func) | (dir,mod,func) <- dirControls
    ]
    ++
    [
     ("M4-r", prevWS),
     ("M4-s", nextWS),
     ("M1-z", toggleWS),
     ---------- layout management ----------
     ("M4-<Space>", sendMessage NextLayout),
     --("M4-S-<Space>", sendMessage PrevLayout),
     ("M4-M1-<Space>", setLayout $ layoutHook conf),
     ("M4-z", sendMessage (Toggle "full")),
     ("M4-x", sendMessage Shrink),
     ("M4-c", sendMessage Expand),
     ("M4-S-x", sendMessage (IncMasterN 1)),
     ("M4-S-c", sendMessage (IncMasterN (-1))),
     ("M4-M1-x", sendMessage MirrorExpand),
     ("M4-M1-c", sendMessage MirrorShrink),
     ("M4-v", sendMessage ToggleStruts),
     ---------- window management ----------
     ("M4-w", windows W.focusUp),
     ("M4-f", windows W.focusDown),
     ("M4-S-w", windows W.swapUp),
     ("M4-S-f", windows W.swapDown),
     ("M4-q", windows W.focusMaster),
     ("M4-a", windows W.swapMaster),
     ("M4-t", withFocused $ windows . W.sink),
     ("M4-<Esc>", kill),
     ---------- spawning ----------
     ("M4-<Tab>", spawn $ terminal conf),
     ("M1-`", spawn myRun),
     ("M1-<F1>", namedScratchpadAction myScratchPads termName),
     ("M1-<F2>", namedScratchpadAction myScratchPads calcName),
     ("M1-<F3>", namedScratchpadAction myScratchPads wifiName),
     ("M1-<F4>", namedScratchpadAction myScratchPads htopName),
     ("M1-<F5>", namedScratchpadAction myScratchPads mixerName),
     ---------- media keys ----------
     ("<XF86AudioMute>", spawn (script ++ "volume toggle")),
     ("<XF86AudioLowerVolume>", spawn (script ++ "volume dec")),
     ("<XF86AudioRaiseVolume>", spawn (script ++ "volume inc")),
     ("<XF86MonBrightnessDown>", spawn (script ++ "light dec")),
     ("<XF86MonBrightnessUp>", spawn (script ++ "light inc")),
     ("<XF86Display>", spawn "arandr"),
     ("<XF86Search>", spawn myRun),
     ---------- testing media keys ----------
     ("<XF86AudioMicMute>", spawn (script ++ "volume toggle")),
     ("<XF86Tools>", spawn (script ++ "volume toggle")),
     ("<XF86LaunchA>", spawn (script ++ "volume toggle")),
     ("<XF86MyComputer>", spawn (script ++ "volume toggle")),
     ---------- misc ----------
     ("C-/", spawn (script ++ "volume toggle")),
     ("C-<D>", spawn (script ++ "volume dec")),
     ("C-<U>", spawn (script ++ "volume inc")),
     ("C-S-<D>", spawn (script ++ "volume min")),
     ("C-S-<U>", spawn (script ++ "volume max")),
     ("C-S-/", spawn (script ++ "volume med")),
     ("C-m", spawn (script ++ "mic-toggle")),
     ("M1-<D>", spawn (script ++ "light dec")),
     ("M1-<U>", spawn (script ++ "light inc")),
     ("M1-S-<U>", spawn (script ++ "light max")),
     ("M1-S-<D>", spawn (script ++ "light dim")),
     ("M1-S-/", spawn (script ++ "light med")),
     ("M1-/", spawn (script ++ "light toggle")),
     ("M4-\\", spawn (script ++ "print")),
     ("M4-M1-\\", spawn (script ++ "print -s")),
     ("M1-,", spawn (script ++ "bg-slides")),
     ("M4-M1-;", spawn (script ++ "touchpad-toggle")),
     ("M4-M1-=", spawn (script ++ "screen-toggle")),
     ("M4-/", windows copyToAll),
     ("M4-S-/", killAllOtherCopies),
     ---------- quit ----------
     ("C-M4-<Backspace>", io (exitWith ExitSuccess))
    ]

---------------------------------------------------------------------------------
-- cursor actions

myMouseBindings (XConfig {}) = fromList $
  [
    ((mod1Mask, button1), (\w -> XMonad.focus w >> mouseMoveWindow w)),
    ((mod1Mask, button3), (\w -> XMonad.focus w >> Flex.mouseResizeWindow w))
  ]

---------------------------------------------------------------------------------
-- run XMonad

main = do
  xmonad $ xfceConfig {
        terminal = myTerminal,
        borderWidth = myBorderWidth,
        normalBorderColor = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        focusFollowsMouse = myFocusFollowsMouse,
        modMask = mod4Mask,
        XMonad.keys =  myKeys,
        XMonad.workspaces = myWorkspaces,
        layoutHook = myLayoutHook,
        manageHook = namedScratchpadManageHook myScratchPads
                     <+> placeHook myPlacement
                     <+> myManageHook <+> manageDocks
                     <+> fullscreenManageHook
                     <+> manageHook defaultConfig,
        mouseBindings = myMouseBindings,
        startupHook = ewmhDesktopsStartup >> setWMName "LG3D",
        handleEventHook = ewmhDesktopsEventHook <+> FS.fullscreenEventHook
    }
