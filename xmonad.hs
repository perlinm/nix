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
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Loggers
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.NamedScratchpad

-----------------------------------------------------------------------
-- Variables

myTerminal = "xfce4-terminal"
myAppFinder = "xfce4-appfinder"

fullBlack = "#000000"
fullWhite = "#ffffff"
black = "#1a1a1a"
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
infoBar = script ++ barScript ++ " -l 'xmonad' | dzen2 -h " ++ show barHeight
          ++ " -fn " ++ barFont ++ " -e 'onstart=lower;button1=lower'"

killBar = "killall dzen2 stalonetray " ++ barScript ++ " 2> /dev/null"
restartCMD = "/usr/bin/xmonad --recompile && /usr/bin/xmonad --restart"


-----------------------------------------------------------------------
-- Window rules

myManageHook = composeAll . concat $
  [
    [ className =? c --> doCenterFloat  | c <- myFloats ]
  ]
  where
    myFloats = ["Xfce4-appfinder","Nm-connection-editor",
               "Nm-openconnect-auth-dialog"," ","Wicd-client.py"]

-----------------------------------------------------------------------
-- Scratchpads

myScratchPads = [ NS "sage-calc" spawnCalc findCalc manageCalc,
                  NS "vol-control" spawnMixer findMixer manageMixer,
                  NS "htop-term" spawnHtop findHtop manageHtop ]
  where
    spawnCalc = (script ++ "sage-calc")
    findCalc = title =? "sage-calc"
    manageCalc = customFloating $ W.RationalRect l t w h
      where
        h = 2/3
        w = 1/3
        t = 1/25
        l = 1-w
    spawnMixer = (script ++ "vol-control")
    findMixer = title =? "vol-control"
    manageMixer = customFloating $ W.RationalRect l t w h
      where
        h = 3/4
        w = 2/3
        t = (1-h)/2
        l = (1-w)/2
    spawnHtop = (script ++ "htop-term")
    findHtop = title =? "htop-term"
    manageHtop = customFloating $ W.RationalRect l t w h
      where
        h = 4/5
        w = 1/2
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
  spawn (script ++ "light 60")
  spawn (script ++ "volume off")
  spawn (script ++ "volume med")


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

a = mod1Mask
w = mod4Mask
s = shiftMask
c = controlMask
numRow = ([xK_grave]++[xK_1..xK_9]++[xK_0])

myKeys conf@(XConfig {XMonad.modMask = w}) = M.fromList $
  [
    ---------- Workspace management ----------
    ((w .|. m, k), windows $ f i) |
    (i,k) <- zip (XMonad.workspaces conf) numRow,
    (f, m) <- [(W.greedyView, 0), (W.shift, s),
               (\i -> W.greedyView i . W.shift i, a)]
  ]
  ++
  [
    ((w .|. c, k), swapWithCurrent i) |
    (i, k) <- zip (XMonad.workspaces conf) numRow
  ]
  ++
  [
    ((w, xK_Left), moveTo Prev (WSIs notNSP)),
    ((w, xK_Right), moveTo Next (WSIs notNSP)),
    ((w .|. c, xK_Left), swapTo' Prev (WSIs notNSP)),
    ((w .|. c, xK_Right), swapTo' Next (WSIs notNSP)),
    ((w .|. s, xK_Left), shiftTo Prev (WSIs notNSP)),
    ((w .|. s, xK_Right), shiftTo Next (WSIs notNSP)),
    ((w .|. a, xK_Left), shiftTo Prev (WSIs notNSP) >> moveTo Prev (WSIs notNSP)),
    ((w .|. a, xK_Right), shiftTo Next (WSIs notNSP) >> moveTo Next (WSIs notNSP)),
    ((w, xK_n), moveTo Prev (WSIs notNSP)),
    ((w, xK_i), moveTo Next (WSIs notNSP)),
    ((w .|. c, xK_n), swapTo' Prev (WSIs notNSP)),
    ((w .|. c, xK_i), swapTo' Next (WSIs notNSP)),
    ((w .|. s, xK_n), shiftTo Prev (WSIs notNSP)),
    ((w .|. s, xK_i), shiftTo Next (WSIs notNSP)),
    ((w .|. a, xK_n), shiftTo Prev (WSIs notNSP) >> moveTo Prev (WSIs notNSP)),
    ((w .|. a, xK_i), shiftTo Next (WSIs notNSP) >> moveTo Next (WSIs notNSP)),
    ((a, xK_z), toggleWS' ["NSP"]),
    ---------- Layout management ----------
    ((w, xK_space), sendMessage NextLayout),
    --((w .|. s, xK_space), sendMessage PrevLayout),
    ((w .|. a, xK_space), setLayout $ XMonad.layoutHook conf),
    ((w, xK_z), sendMessage (Toggle FULL)),
    ((w, xK_r), sendMessage (IncMasterN 1)),
    ((w, xK_s), sendMessage (IncMasterN (-1))),
    ((w, xK_x), sendMessage Shrink),
    ((w, xK_c), sendMessage Expand),
    ((w .|. a, xK_x), sendMessage MirrorExpand),
    ((w .|. a, xK_c), sendMessage MirrorShrink),
    ((w, xK_v), sendMessage ToggleStruts),
    ---------- Window management ----------
    ((w, xK_w), windows W.focusUp),
    ((w, xK_f), windows W.focusDown),
    ((w .|. s, xK_w),windows W.swapUp),
    ((w .|. s, xK_f),windows W.swapDown),
    ((w, xK_q), windows W.focusMaster),
    ((w, xK_a), windows W.swapMaster),
    --((w, xK_u), sendMessage $ Go U),
    --((w, xK_e), sendMessage $ Go D),
    --((w, xK_n), sendMessage $ Go L),
    --((w, xK_i), sendMessage $ Go R),
    --((w .|. s, xK_u), sendMessage $ Swap U),
    --((w .|. s, xK_e), sendMessage $ Swap D),
    --((w .|. s, xK_n), sendMessage $ Swap L),
    --((w .|. s, xK_i), sendMessage $ Swap R),
    ((w, xK_t), withFocused $ windows . W.sink),
    ((w, xK_Escape), kill),
    ---------- Spawn ----------
    ((w, xK_Tab), spawn $ XMonad.terminal conf),
    ((a, xK_grave), spawn myRun),
    ((a .|. c, xK_grave), spawn myAppFinder),
    ((a, xK_F1), namedScratchpadAction myScratchPads "sage-calc"),
    ((a, xK_F2), namedScratchpadAction myScratchPads "vol-control"),
    ((a, xK_F3), namedScratchpadAction myScratchPads "htop-term"),
    ---------- Restart ----------
    ((w .|. a, xK_BackSpace), spawn killBar),
    ((w, xK_BackSpace), spawn restartCMD),
    ---------- Misc ----------
    ((c, xK_slash), spawn (script ++ "volume toggle")),
    ((c, xK_Up), spawn (script ++ "volume inc")),
    ((c, xK_Down), spawn (script ++ "volume dec")),
    ((c .|. s, xK_Up), spawn (script ++ "volume max")),
    ((c .|. s, xK_Down), spawn (script ++ "volume min")),
    ((c .|. s, xK_slash), spawn (script ++ "volume med")),
    ((a, xK_Up), spawn (script ++ "light max")),
    ((a, xK_Down), spawn (script ++ "light dim")),
    ((a .|. s, xK_Up), spawn (script ++ "light inc")),
    ((a .|. s, xK_Down), spawn (script ++ "light dec")),
    ((w, xK_backslash), spawn (script ++ "print")),
    ((w .|. a, xK_backslash), spawn (script ++ "print --select")),
    ((a, xK_slash), spawn (script ++ "abg swap")),
    ((a, xK_apostrophe), spawn (script ++ "abg remove")),
    ((a, xK_period), spawn (script ++ "abg next")),
    ((a, xK_comma), spawn (script ++ "abg last")),
    ((a, xK_semicolon), spawn (script ++ "touchpad-toggle")),
    ((c .|. a, xK_Up), spawn (script ++ "orient normal")),
    ((c .|. a, xK_Down), spawn (script ++ "orient inverted")),
    ((c .|. a, xK_Left), spawn (script ++ "orient left")),
    ((c .|. a, xK_Right), spawn (script ++ "orient right")),
    ((w, xK_slash), windows copyToAll),
    ((w .|. s, xK_slash), killAllOtherCopies),
    ((c .|. a, xK_space), windowMenu),
    ((c .|. s, xK_space), goToSelected defaultGSConfig),
    ((c .|. a .|. s, xK_z), spawn "sus")
  ]

-----------------------------------------------------------------------
-- Cursor actions

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    ((a, button1), (\w -> XMonad.focus w >> mouseMoveWindow w)),
    ((w .|. a, button1), (\w -> XMonad.focus w >> Flex.mouseResizeWindow w))
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
        XMonad.workspaces = myWorkspaces,
        layoutHook = myLayoutHook,
        manageHook = namedScratchpadManageHook myScratchPads
                     <+> placeHook myPlacement
                     <+> myManageHook <+> manageDocks
                     <+> FS.fullscreenManageHook
                     <+> manageHook defaultConfig,
        XMonad.keys = myKeys,
        mouseBindings = myMouseBindings,
        logHook = myLog infoBar,
        startupHook = myStartupHook
                      <+> ewmhDesktopsStartup >> setWMName "LG3D",
        handleEventHook = ewmhDesktopsEventHook <+> FS.fullscreenEventHook
    }
