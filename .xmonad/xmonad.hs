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
            , focusedBorderColor = "#aaaaaa"
            , normalBorderColor = "#333333"
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

myModMask = mod4Mask

myKeys =  
    [ ((0, 0x1008ff11), spawn "amixer -q set Master 1-")
    , ((0, 0x1008ff12), spawn "amixer set Front toggle")
    , ((0, 0x1008ff13), spawn "amixer -q set Master 1+")
    , ((0, 0x1008ff14), spawn "mpc toggle")
    , ((0, 0x1008ff15), spawn "mpc stop")
    , ((0, 0x1008ff16), spawn "mpc prev")
    , ((0, 0x1008ff17), spawn "mpc next")

    , ((myModMask .|. shiftMask, xK_b), bringMenu)
    , ((myModMask .|. shiftMask, xK_n), refresh)

    , ((0, xK_Print), spawn "scrot")
    , ((shiftMask, xK_Print), spawn "sleep 0.2; scrot -s")
    ]

myWorkspaces = ["1", "2", "3", "4",
                "5", "6", "7", "8",
                "9"]

associate :: WorkspaceId -> String -> ManageHook
associate area wmClass = className =? wmClass --> doShift area

myManageHook =
    [ className =? "MPlayer" --> doFloat
    , className =? "VLC media player" --> doFloat
    ]
