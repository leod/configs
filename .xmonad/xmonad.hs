import Data.List
import Data.Ratio
import qualified Data.Map as M
import System.IO

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Scratchpad
import XMonad.Layout
import XMonad.Layout.LayoutHints
import XMonad.Layout.HintedTile
import XMonad.Layout.Spiral
import XMonad.Actions.MouseGestures
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowBringer

main = do 
    xmproc <- spawnPipe "xmobar"
    xmonad $ withUrgencyHook NoUrgencyHook defaultConfig
            {
              terminal = "urxvt"
            , focusedBorderColor = "#ff0000"
            , workspaces = myWorkspaces
            , manageHook = manageDocks <+> manageHook defaultConfig <+> composeAll myManageHook
            , layoutHook = smartBorders $ avoidStruts $ layoutHook defaultConfig
            , logHook = dynamicLogWithPP $ xmobarPP
                          { ppOutput = hPutStrLn xmproc
                          , ppTitle = xmobarColor "green" "" . shorten 100 
                          , ppSep = " | "
                          , ppLayout = id
                          , ppUrgent = xmobarColor "grey" "red" . wrap "!" "!" . xmobarStrip
                          }
            , modMask = myModMask
            }
            `additionalKeys` myKeys
            `additionalMouseBindings` myMouseBindings

gestures = M.fromList
    [ ([R], const nextWS)
    , ([L], const prevWS)
    ]

myModMask = mod4Mask

myMouseBindings =
    [ ((mod4Mask, button2), mouseGesture gestures) ]

myKeys =  
    [ ((0, 0x1008ff11), spawn "amixer -q set Master 5-")
    , ((0, 0x1008ff12), spawn "amixer set Front toggle")
    , ((0, 0x1008ff13), spawn "amixer -q set Master 5+")
    {-, ((0, 0x1008ff14), spawn "dbus-send --type=method_call --dest=org.kde.amarok /Player org.freedesktop.MediaPlayer.Pause")-}
    {-, ((0, 0x1008ff15), spawn "dbus-send --type=method_call --dest=org.kde.amarok /Player org.freedesktop.MediaPlayer.Stop")-}
    {-, ((0, 0x1008ff16), spawn "dbus-send --type=method_call --dest=org.kde.amarok /Player org.freedesktop.MediaPlayer.Prev")-}
    {-, ((0, 0x1008ff17), spawn "dbus-send --type=method_call --dest=org.kde.amarok /Player org.freedesktop.MediaPlayer.Next")-}
    , ((0, 0x1008ff14), spawn "mpc toggle")
    , ((0, 0x1008ff15), spawn "mpc stop")
    , ((0, 0x1008ff16), spawn "mpc prev")
    , ((0, 0x1008ff17), spawn "mpc next")

    , ((myModMask .|. shiftMask, xK_b), bringMenu)

    , ((myModMask .|. shiftMask, xK_n), refresh)

    -- Screenshots
    , ((0, xK_Print), spawn "scrot")
    , ((shiftMask, xK_Print), spawn "sleep 0.2; scrot -s")
    ]

xmobarStrip :: String -> String
xmobarStrip = strip [] where
    strip keep x
      | null x                 = keep
      | "<fc="  `isPrefixOf` x = strip keep (drop 1 . dropWhile (/= '>') $ x)
      | "</fc>" `isPrefixOf` x = strip keep (drop 5  x)
      | '<' == head x          = strip (keep ++ "<") (tail x)
      | otherwise              = let (good,x') = span (/= '<') x
                                 in strip (keep ++ good) x'

-- I disabled names as they bloat my xmobar too much
-- myWorkspaces = ["1:web", "2:prog", "3:term", "4:stuff", -- First screen
--                 "5:im", "6:mus", "7:stuff", "8:min"]    -- Second screen

myWorkspaces = ["1", "2", "3", "4",
                "5", "6", "7", "8"]

associate :: WorkspaceId -> String -> ManageHook
associate area wmClass = className =? wmClass --> doShift area

myManageHook =
    [ associate "Amerokapp" "6"
    , associate "Pidgin" "5"
    , className =? "MPlayer" --> doFloat
    , className =? "VLC media player" --> doFloat
    ]
