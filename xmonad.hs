import Data.Map as M
import System.IO
import System.Exit
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.CopyWindow
import XMonad.Actions.FlexibleResize as Flex
import XMonad.Actions.GridSelect
import XMonad.Actions.NoBorders
import XMonad.Actions.WindowMenu
import XMonad.Actions.WorkspaceNames
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Hooks.SetWMName
import XMonad.Layout.Fullscreen as FS
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.WindowNavigation
import XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.NamedScratchpad

-----------------------------------------------------------------------
-- Variables

myTerminal = "xfce4-terminal"
myAppFinder = "xfce4-appfinder"

fullBlack = "#000000"
fullWhite = "#ffffff"
black = "#111111"
white = "#dddddd"
grey = "#777777"
red = "#c11b17"
green = "#347c17"
blue = "#00688b"
yellow = "#ffbb00"
barFont = "monospace-9"
barHeight = 16

myBorderWidth = 1
myNormalBorderColor = fullBlack
myFocusedBorderColor = grey
myFocusFollowsMouse = True
myWorkspaces = Prelude.map show [0..10]

script = "/home/perlinm/scripts/"

myRun = "$(yeganesh -x --"
        ++ " -nb '" ++ black ++ "'"
        ++ " -nf '" ++ white ++ "'" ++ " -sf '" ++ white ++ "'"
        ++ " -fn " ++ barFont ++ ")"

barScript = "dzen-bar.py"
infoBar = "nice -n -10 " ++ script ++ barScript ++ " | dzen2 -h " ++ show barHeight
          ++ " -fn " ++ barFont ++ " -e 'onstart=lower;button1=lower'"

killBar = "killall dzen2 stalonetray " ++ barScript ++ " 2> /dev/null"
restartCMD = "/usr/bin/xmonad --recompile && /usr/bin/xmonad --restart"

-----------------------------------------------------------------------
-- Window rules

myManageHook = composeAll . concat $
  [
    [ (className =? c <&&> title /=? t) --> doCenterFloat
          | c <- cFloats, t <- tSinks ]
  ]
  where
    cFloats = ["Xfce4-appfinder","Nm-connection-editor",
               "Nm-openconnect-auth-dialog"," ","Wicd-client.py",
               "Python2", "MATLAB", "com-mathworks-util-PostVMInit",
               "Toplevel"]
    tSinks = ["MATLAB R2012b"]

-----------------------------------------------------------------------
-- Scratchpads

termName = "term"
calcName = "calc"
wifiName = "wicd-curses"
htopName = "htop"
mixerName = "mixer"
myScratchPads = [ NS termName spawnTerm findTerm manageTerm,
                  NS calcName spawnCalc findCalc manageCalc,
                  NS wifiName spawnWifi findWifi manageWifi,
                  NS htopName spawnHtop findHtop manageHtop,
                  NS mixerName spawnMixer findMixer manageMixer ]
  where
    spawnTerm = (script ++ "pads " ++ termName)
    findTerm = title =? ("pad-" ++ termName)
    manageTerm = customFloating $ W.RationalRect l t w h
      where
        h = 1/2
        w = 0.45
        t = (1-h)*9/10
        l = (1/2-w)/2
    spawnCalc = (script ++ "pads " ++ calcName)
    findCalc = title =? ("pad-" ++ calcName)
    manageCalc = customFloating $ W.RationalRect l t w h
      where
        h = 2/3
        w = 2/5
        t = 1/25
        l = 1-w
    spawnWifi = (script ++ "pads " ++ wifiName)
    findWifi = title =? ("pad-" ++ wifiName)
    manageWifi = customFloating $ W.RationalRect l t w h
      where
        h = 1/2
        w = 1/2
        t = (1-h)/2
        l = (1-w)/2
    spawnHtop = (script ++ "pads " ++ htopName)
    findHtop = title =? ("pad-" ++ htopName)
    manageHtop = customFloating $ W.RationalRect l t w h
      where
        h = 4/5
        w = 1/2
        t = (1-h)/2
        l = (1-w)/2
    spawnMixer = (script ++ "pads " ++ mixerName)
    findMixer = title =? ("pad-" ++ mixerName)
    manageMixer = customFloating $ W.RationalRect l t w h
      where
        h = 3/4
        w = 2/3
        t = (1-h)/2
        l = (1-w)/2

-----------------------------------------------------------------------
-- Workspace info log

myLog info = dynamicLogWithPP $ defaultPP {
  ppCurrent = wrap "c " "",
  ppVisible = wrap "v " "",
  ppHidden = wrap "h " "" . noScratchPad,
  ppHiddenNoWindows = wrap "hnw " "" . noScratchPad,
  ppUrgent = wrap "u " "",
  ppOrder = \(ws:l:t:_) -> [ws,l,t],
  ppSep = "|:|",
  ppWsSep = "|",
  ppOutput = hPutStrLn info
}
  where
    noScratchPad ws = if ws == "NSP" then "" else ws

-----------------------------------------------------------------------
-- Startup commands

myStartupHook :: X()
myStartupHook = do
  spawn (script ++ "bg-slides")

-----------------------------------------------------------------------
-- Layout definitions

normal = renamed [Replace "Normal"] $ ResizableTall 1 (1/50) (1/2) []
emacs = renamed [Replace "Emacs"] $ ResizableTall 1 (1/50) (2/5) []
chat = renamed [Replace "Chat"] $ ResizableTall 1 (1/50) (3/4) []
skype = renamed [Replace "Skype"] $ ResizableTall 1 (1/50) (63/100) []
myLayoutHook = smartBorders $ avoidStruts $ windowNavigation $
    mkToggle (single FULL)
  ( normal ||| emacs ||| chat ||| skype )

myPlacement = withGaps (16,16,16,16) (fixed (0.5,0.5))

-----------------------------------------------------------------------
-- Key commands

notNSP = (return $ ("NSP" /=) . W.tag) :: X (WindowSpace -> Bool)

numRow = ["`"] ++ (Prelude.map show [1..9]) ++ ["0"]
myKeys = \conf -> mkKeymap conf $
    ---------- Workspace management ----------
    [
     ("M4" ++ mod ++ "-" ++ key, windows $ func ws) |
     (ws,key) <- zip myWorkspaces numRow,
              (func,mod) <- [(W.greedyView, ""), (W.shift, "-S"),
                             (\i -> W.greedyView i . W.shift i,"-M1")]
    ]
    ++
    [
     ("M4-C-" ++ key, swapWithCurrent ws) |
     (ws,key) <- zip myWorkspaces numRow
    ]
    ++
    [
     ("M4-p", moveTo Prev (WSIs notNSP)),
     ("M4-g", moveTo Next (WSIs notNSP)),
     ("M4-<L>", moveTo Prev (WSIs notNSP)),
     ("M4-<R>", moveTo Next (WSIs notNSP)),
     ("M4-C-<L>", swapTo' Prev (WSIs notNSP)),
     ("M4-C-<R>", swapTo' Next (WSIs notNSP)),
     ("M4-S-<L>", shiftTo Prev (WSIs notNSP)),
     ("M4-S-<R>", shiftTo Next (WSIs notNSP)),
     ("M4-M1-<L>", shiftTo Prev (WSIs notNSP) >> moveTo Prev (WSIs notNSP)),
     ("M4-M1-<R>", shiftTo Next (WSIs notNSP) >> moveTo Next (WSIs notNSP)),
     ("M4-n", moveTo Prev (WSIs notNSP)),
     ("M4-i", moveTo Next (WSIs notNSP)),
     ("M4-C-n", swapTo' Prev (WSIs notNSP)),
     ("M4-C-i", swapTo' Next (WSIs notNSP)),
     ("M4-S-n", shiftTo Prev (WSIs notNSP)),
     ("M4-S-i", shiftTo Next (WSIs notNSP)),
     ("M4-M1-n", shiftTo Prev (WSIs notNSP) >> moveTo Prev (WSIs notNSP)),
     ("M4-M1-i", shiftTo Next (WSIs notNSP) >> moveTo Next (WSIs notNSP)),
     ("M1-z", toggleWS' ["NSP"]),
    ---------- Layout management ----------
    ("M4-<Space>", sendMessage NextLayout),
    --((w .|. s, space), sendMessage PrevLayout),
    ("M4-M1-<Space>", setLayout $ XMonad.layoutHook conf),
    ("M4-z", sendMessage (Toggle FULL)),
    ("M4-r", sendMessage (IncMasterN 1)),
    ("M4-s", sendMessage (IncMasterN (-1))),
    ("M4-x", sendMessage Shrink),
    ("M4-c", sendMessage Expand),
    ("M4-M1-x", sendMessage MirrorExpand),
    ("M4-M1-c", sendMessage MirrorShrink),
    ("M4-v", sendMessage ToggleStruts),
    ---------- Window management ----------
    ("M4-w", windows W.focusUp),
    ("M4-f", windows W.focusDown),
    ("M4-S-w", windows W.swapUp),
    ("M4-S-f", windows W.swapDown),
    ("M4-q", windows W.focusMaster),
    ("M4-a", windows W.swapMaster),
    ("M4-t", withFocused $ windows . W.sink),
    ("M4-<Esc>", kill),
    ---------- Spawn ----------
    ("M4-<Tab>", spawn $ XMonad.terminal conf),
    ("M1-`", spawn myRun),
    ("M1-C-`", spawn myAppFinder),
    ("M1-<F1>", namedScratchpadAction myScratchPads termName),
    ("M1-<F2>", namedScratchpadAction myScratchPads calcName),
    ("M1-<F3>", namedScratchpadAction myScratchPads wifiName),
    ("M1-<F4>", namedScratchpadAction myScratchPads htopName),
    ("M1-<F5>", namedScratchpadAction myScratchPads mixerName),
    ---------- Restart ----------
    ("M4-M1-<Backspace>", spawn killBar),
    ("M4-<Backspace>", spawn restartCMD),
    ---------- Misc ----------
    ("C-/", spawn (script ++ "volume toggle")),
    ("C-<U>", spawn (script ++ "volume inc")),
    ("C-<D>", spawn (script ++ "volume dec")),
    ("C-S-<U>", spawn (script ++ "volume max")),
    ("C-S-<D>", spawn (script ++ "volume min")),
    ("C-S-/", spawn (script ++ "volume med")),
    ("C-m", spawn (script ++ "mic-toggle")),
    ("M1-<U", spawn (script ++ "light inc")),
    ("M1-<D>", spawn (script ++ "light dec")),
    ("M1-S-<U>", spawn (script ++ "light max")),
    ("M1-S-<D>", spawn (script ++ "light dim")),
    ("M1-S-/", spawn (script ++ "light med")),
    ("M1-/", spawn (script ++ "light toggle")),
    ("M4-\\", spawn (script ++ "print")),
    ("M4-M1-\\", spawn (script ++ "print --select")),
    ("M1-,", spawn (script ++ "bg-slides")),
    ("M1-;", spawn (script ++ "touchpad-toggle")),
    ("M1-C-<U>", spawn (script ++ "orient normal")),
    ("M1-C-<D>", spawn (script ++ "orient inverted")),
    ("M1-C-<L>", spawn (script ++ "orient left")),
    ("M1-C-<R>", spawn (script ++ "orient right")),
    ("M4-/", windows copyToAll),
    ("M4-S-/", killAllOtherCopies),
    ("C-M1-<Space>", windowMenu),
    ("C-S-<Space>", goToSelected defaultGSConfig),
    ("M1-C-S-z", spawn "sus")
    ]

-----------------------------------------------------------------------
-- Cursor actions

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    ((mod1Mask, button1), (\w -> XMonad.focus w >> mouseMoveWindow w)),
    ((mod4Mask .|. mod1Mask, button1),
     (\w -> XMonad.focus w >> Flex.mouseResizeWindow w))
  ]

-----------------------------------------------------------------------
-- Run XMonad

main = do
  infoBar <- spawnPipe infoBar
  xmonad $ defaultConfig {
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
                     <+> FS.fullscreenManageHook
                     <+> manageHook defaultConfig,
        mouseBindings = myMouseBindings,
        logHook = myLog infoBar,
        startupHook = myStartupHook
                      <+> ewmhDesktopsStartup >> setWMName "LG3D",
        handleEventHook = ewmhDesktopsEventHook <+> FS.fullscreenEventHook
    }
