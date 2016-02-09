import Control.Monad
import Data.Map
import System.Exit
import System.IO
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.ConstrainedResize as CR
import XMonad.Actions.CopyWindow
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
import XMonad.Layout.Spacing
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

home = "/home/perlinm/"
script = home ++ "scripts/"

myTerminal = "xfce4-terminal"
myRun = "bashrun"
myFinder = "xfce4-appfinder"

---------------------------------------------------------------------------------
-- window rules

myManageHook = composeAll . concat $
  [
    [ isDialog--> doF W.shiftMaster <+> doF W.swapDown ],
    [ (roleName =? r) --> doCenterFloat | r <- floatRole ],
    -- [ (title =? t) --> sink | t <- sinkTitle ],
    [ (roleName =? r) --> sink | r <- sinkRole ],
    [ (className =? c) --> doIgnore | c <- ignoreClass ],
    [ (className =? c) --> doCenterFloat | c <- floatClass ]
  ]
  where
    roleName = stringProperty "WM_WINDOW_ROLE"
    sink = (ask >>= doF . W.sink) <+> doF W.swapDown
    floatRole = ["browser"]
    -- sinkTitle = ["Hangouts"]
    sinkRole = ["pop-up"]
    ignoreClass = ["Xfce4-notifyd"]
    floatClass = ["Xfce4-appfinder","Xfce4-panel","Nm-connection-editor",
                  "Nm-openconnect-auth-dialog"," ","Wicd-client.py","Python2",
                  "Thunar","Arandr","Wrapper-1.0","XTerm",
                  "Desmume","Nds","Gvbam","Vba"]

---------------------------------------------------------------------------------
-- scratchpads

termName = "term"
calcName = "calc"
altTermName = "alt-term"
htopName = "htop"
mixerName = "mixer"
myScratchPads = [ NS termName spawnTerm findTerm manageTerm,
                  NS calcName spawnCalc findCalc manageCalc,
                  NS altTermName spawnAltTerm findAltTerm manageAltTerm,
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
    spawnAltTerm = script ++ "pads " ++ altTermName
    findAltTerm = title =? ("pad-" ++ altTermName)
    manageAltTerm = customFloating $ W.RationalRect l t w h
      where
        h = 1/2
        w = 0.45
        t = (1-h)*9/10
        l = 1/2+(1/2-w)/2
    spawnCalc = script ++ "pads " ++ calcName
    findCalc = title =? ("pad-" ++ calcName)
    manageCalc = customFloating $ W.RationalRect l t w h
      where
        h = 2/3
        w = 2/5
        t = 1/25
        l = 1-w
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
latex = renamed [Replace "latex"] $ ResizableTall 1 (1/50) (39/100) []
chat = renamed [Replace "chat"] $ ResizableTall 1 (1/50) (73/100) []
skype = renamed [Replace "skype"] $ ResizableTall 1 (1/50) (64/100) []
full = renamed [Replace "full"] $ Full

(|+|) :: (LayoutClass l a, LayoutClass r a) =>
         (l a, Int) -> r a -> (Choose l r a, Int)
(a,n) |+| b = (a ||| b, n+1)
countedLayouts = (normal,1) |+| latex |+| chat |+| skype
layouts = fst countedLayouts
layoutCount = snd countedLayouts

myLayoutHook = smartBorders $ avoidStruts $ windowNavigation $ smartSpacing 3 $
               toggleLayouts full layouts

myPlacement = withGaps (16,16,16,16) (fixed (0.5,0.5))

---------------------------------------------------------------------------------
-- key commands

-- define number row, arrow keys, and modification keys
numRow = ["`"] ++ (Prelude.map show [1..9]) ++ ["0","-"]
arrows = [["<L>","<R>","<U>","<D>"],["n","i","u","e"],["r","s","p","v"]]
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
     ("M4-<Space>", sendMessage NextLayout),
     ("M4-S-<Space>", do { replicateM_ (layoutCount-1) $ sendMessage NextLayout}),
     ("M4-M1-<Space>", setLayout $ layoutHook conf),
     ("M4-z", sendMessage (Toggle "full")),
     ("M4-x", sendMessage Shrink),
     ("M4-c", sendMessage Expand),
     ("M4-S-x", sendMessage (IncMasterN 1)),
     ("M4-S-c", sendMessage (IncMasterN (-1))),
     ("M4-M1-x", sendMessage MirrorExpand),
     ("M4-M1-c", sendMessage MirrorShrink),
     ("M4-b", sendMessage ToggleStruts),
     ---------- window management ----------
     ("M4-w", windows W.focusUp),
     ("M4-f", windows W.focusDown),
     ("M4-S-w", windows W.swapUp),
     ("M4-S-f", windows W.swapDown),
     ("M4-q", windows W.focusMaster),
     ("M4-a", windows W.shiftMaster),
     ("M4-t", withFocused $ windows . W.sink),
     ("M4-<Esc>", kill),
     ---------- spawning ----------
     ("M4-<Tab>", spawn $ terminal conf),
     ("M1-`", spawn myRun),
     ("M1-C-`", spawn myFinder),
     ("M1-<F1>", namedScratchpadAction myScratchPads termName),
     ("M1-<F2>", namedScratchpadAction myScratchPads altTermName),
     ("M1-<F3>", namedScratchpadAction myScratchPads calcName),
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
     ---------- volume ----------
     ("C-/", spawn (script ++ "volume toggle")),
     ("C-<D>", spawn (script ++ "volume dec")),
     ("C-<U>", spawn (script ++ "volume inc")),
     ("C-S-<D>", spawn (script ++ "volume min")),
     ("C-S-<U>", spawn (script ++ "volume max")),
     ("C-S-/", spawn (script ++ "volume med")),
     ---------- backlight ----------
     ("M1-<D>", spawn (script ++ "light dec")),
     ("M1-<U>", spawn (script ++ "light inc")),
     ("M1-S-<U>", spawn (script ++ "light max")),
     ("M1-S-<D>", spawn (script ++ "light dim")),
     ("M1-S-/", spawn (script ++ "light med")),
     ---------- screenshots ----------
     ("M4-\\", spawn (script ++ "print")),
     ("M4-M1-\\", spawn (script ++ "print -s")),
     ---------- misc ----------
     ("M1-;", spawn (script ++ "touchpad-toggle")),
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
    ((mod1Mask, button3), (\w -> XMonad.focus w >> Flex.mouseResizeWindow w)),
    ((mod1Mask .|. shiftMask, button1), (\w -> XMonad.focus w >> Flex.mouseResizeWindow w))
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
        XMonad.keys = myKeys,
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
