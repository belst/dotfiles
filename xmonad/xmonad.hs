import Control.Monad
import Data.List
import Data.Monoid
import Control.Applicative
import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.Xlib.Extras
import System.Exit
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.RotSlaves
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Place
import XMonad.Hooks.UrgencyHook
import XMonad.Layout
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Monitor
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.TwoPane
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows
import XMonad.Util.Paste
import XMonad.Util.Run
import XMonad.Util.Scratchpad
import XMonad.Util.WindowProperties
import XMonad.Util.WorkspaceCompare
import qualified Data.Map as M
import qualified XMonad.StackSet as W


myWorkspaces :: [WorkspaceId]
myWorkspaces = ["1", "2", "3", "4", "5", "6" ,"7", "8", "9", "0"]

dzenCommand :: String
dzenCommand = "dzen2 -ta l -e  'onstart=lower' -w 683"

dzen2Command :: String
dzen2Command = "time2str | dzen2 -ta r -e 'onstart=lower' -w 683 -x 682"

scrot2Dropbox :: String
scrot2Dropbox = "scrot screen_%Y-%m-%d-%H-%M-%S.png -e 'mv $f ~/Dropbox/Public && dropbox-cli puburl Dropbox/Public/$f | xsel -ib && xsel -ob | xsel -ip'"

scrot2DropboxW :: String
scrot2DropboxW = "scrot window_%Y-%m-%d-%H-%M-%S.png -u -e 'mv $f ~/Dropbox/Public && dropbox-cli puburl Dropbox/Public/$f | xsel -ib && xsel -ob | xsel -ip'"

scrot2DropboxS :: String
scrot2DropboxS = "sleep 0.2; scrot selection_%Y-%m-%d-%H-%M-%S.png -s -e 'mv $f ~/Dropbox/Public && dropbox-cli puburl Dropbox/Public/$f | xsel -ib && xsel -ob | xsel -ip'"

main = do
  d <- spawnPipe dzenCommand
  --e <- spawnPipe dzen2Command
  spawn dzen2Command
  xmonad $withUrgencyHook NoUrgencyHook $defaultConfig { 
  modMask = mod4Mask
  , keys = myKeys
  , layoutHook = myLayoutHook
  , manageHook = myManageHook <+> manageDocks
  , normalBorderColor = "#1C1CFF"
  , workspaces = myWorkspaces
  , handleEventHook = fullscreenEventHook
  , logHook = myLogHook d
    --myLogHook2 e
  }

myLayoutHook = fullscreenFull $avoidStruts  $ noBorders . mkToggle (NOBORDERS ?? FULL ?? EOT)  $ mkToggle (single MIRROR) $ tall |||  twopane
  where
    tall = Tall 1 (3/100) (1/2)
    twopane = TwoPane (3/100) (1/2)


myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $ [
  --Launch launcher and tools
    ((modMask, xK_d), spawn "dmenu_run")
  , ((modMask, xK_Return), spawn "urxvtc")

  --Exit/Kill and Stuff
  , ((modMask .|. shiftMask, xK_e), io exitSuccess )
  , ((modMask .|. shiftMask, xK_q), kill)

  --Do layout stuff
  , ((modMask, xK_space), sendMessage NextLayout)
  , ((modMask, xK_semicolon), rotSlavesUp)
  , ((modMask, xK_Up), sendMessage (IncMasterN 1))
  , ((modMask, xK_Down), sendMessage (IncMasterN (-1)))
  , ((modMask, xK_Right), sendMessage Expand)
  , ((modMask, xK_Left), sendMessage Shrink)

  --Move Focus
  , ((modMask, xK_h), windows W.focusMaster)
  , ((modMask, xK_j), windows W.focusDown)
  , ((modMask, xK_k), windows W.focusUp)
  
  --Move Focused
  , ((modMask .|. shiftMask, xK_h), windows W.swapMaster)
  , ((modMask .|. shiftMask, xK_j), windows W.swapDown)
  , ((modMask .|. shiftMask, xK_k), windows W.swapUp)


  --FloatUtil/ Fullscreen
  , ((modMask .|. shiftMask, xK_space), withFocused toggleFloat)
  , ((modMask, xK_f), do
    hideDzen
    sendMessage $ Toggle FULL)
  , ((modMask, xK_m), sendMessage $ Toggle MIRROR)
  , ((modMask, xK_n), nextScreen)

  --Toogle Dzen
  , ((modMask, xK_b), hideDzen)
  
  --Scratchpad
  , ((modMask, xK_c), namedScratchpadAction scratchpads "weechat")
  , ((modMask, xK_x), spawn "lock")
  , ((modMask, xK_v), pasteSelection)

  --Screenshot
  , ((modMask, xK_s), spawn scrot2Dropbox)
  , ((modMask .|. shiftMask, xK_s), spawn scrot2DropboxW)
  , ((modMask .|. controlMask, xK_s), spawn scrot2DropboxS)

  --Special keys
  , ((0, xF86XK_AudioMute), spawn "notify-send `amixer sset Master toggle | grep \"Mono:\" | cut -d ' ' -f 8`")
  , ((0, xF86XK_AudioRaiseVolume), spawn "notify-send `amixer sset Master 5%+ | grep \"Mono:\" | cut -d ' ' -f 6`")
  , ((0, xF86XK_AudioLowerVolume), spawn "notify-send `amixer sset Master 5%- | grep \"Mono:\" | cut -d ' ' -f 6`")
  , ((0, xF86XK_AudioPlay), spawn "mpc toggle")
  , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 10")
  , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10")
  , ((0, 0x1008ffa9), spawn "synclient TouchpadOff=$(synclient -l | grep -c 'TouchpadOff.*=.*0')")
  , ((0, xF86XK_Calculator), spawn "")

  --MPC stuff
  , ((modMask, xK_r), spawn "mpc del 0")

  --close window
  , ((modMask, xK_q), kill1)

  --Tmp/Debug Stuff
  , ((modMask, xK_w), printWindows)
  ]
  
  --switch Workspace
  ++ [((m .|. modMask, k), windows $ f i) | 
   (i,k) <- zip myWorkspaces ([xK_1 .. xK_9] ++ [xK_0]),
   (f,m) <- [(W.greedyView, 0), (W.shift, shiftMask), (copy, controlMask)]]

--myLogHook::
myLogHook h = dynamicLogWithPP $ namedScratchpadFilterOutWorkspacePP $ defaultPP
  { ppCurrent = dzenColor "#303030" "#909090" . pad
  , ppHidden = dzenColor "#909090" "" . pad
--  , ppHiddenNoWindows = dzenColor "#606060" "" . pad
  , ppLayout = dzenColor "#909090" "" . pad
  , ppUrgent = dzenColor "#ff0000" "" . pad . dzenStrip
  , ppTitle = shorten 100
  , ppWsSep = "|"
  , ppOrder = \(ws:_:t:_) -> [ws,t]
  , ppOutput = hPutStrLn h
  }

--myLogHook2 h = dynamicLogWithPP $ defaultPP
--  { ppExtras =
--      [ date "%H:%M %d.%m.%y"]
--  , ppOrder = \(_:_:_:t) -> t
--  , ppOutput = hPutStrLn h
--  }

myManageHook :: ManageHook  
myManageHook = composeAll 
  [ fullscreenManageHook
  , namedScratchpadManageHook scratchpads
  ]

--conky = monitor {
--    prop = ClassName "Conky"
--  , persistent = True
--  , XMonad.Layout.Monitor.name = "Clock"
--  , rect = Rectangle 1200 1 165 60
--  , visible = True
--  , XMonad.Layout.Monitor.opacity = 1
--  }

toggleFloat :: Window -> X ()
toggleFloat x = do
  y <- runQuery isFloating x
  if not y
  then withFocused $ windows . flip W.float (W.RationalRect 0.25 0.25 0.5 0.5)
  else withFocused $ windows . W.sink


--doCopyToAll = ask >>= doF . \w -> (\ws -> foldr($) ws (map (copyWindow w) myWorkspaces))

hideDzen :: X ()
hideDzen = sendMessage ToggleStruts
  --spawn "pkill -USR1 dzen2"


scratchpads =   [ NS "weechat" "urxvtc -title 'WeeChat 1.3' -e weechat" (title =? "WeeChat 1.3") (customFloating (W.RationalRect 0.1 0.1 0.8 0.8))
    ]

printWindows :: X ()
printWindows = do
    ws <- gets windowset
    let ss = mapM (fmap show . getName) $ W.allWindows ws
    let s = liftM (\x -> foldl (++) "zenity --info --text=\"" x ++ "\"") ss
    join $ liftM spawn s
