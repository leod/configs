import Data.List
import Data.Ratio
import qualified Data.Map as M
import System.IO
import System.Exit

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Scratchpad
import XMonad.Layout
import XMonad.Layout.LayoutHints
import XMonad.Layout.HintedTile
import XMonad.Layout.Spiral
import XMonad.Layout.PerWorkspace
import XMonad.Layout.WorkspaceDir
import XMonad.Actions.MouseGestures
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowBringer
import XMonad.Util.Scratchpad
import XMonad.Prompt
import XMonad.Prompt.Shell

myConf = defaultConfig
            {
              terminal = "urxvt"
            , focusedBorderColor = "#aa0000"
            , normalBorderColor = "#333333"
            , workspaces = myWorkspaces
            , keys = myKeys
            , manageHook = manageDocks <+> manageHook defaultConfig <+> composeAll myManageHook
            , layoutHook = myLayoutHook
            , modMask = myModMask
            }
            `additionalKeys` myAddKeys

myModMask = mod4Mask

myManageHook = concat $
    [ [fmap (float `isPrefixOf`) resource --> doCenterFloat | float <- myFloats]
    , [ scratchpadManageHookDefault
      --, isFullscreen --> doFullFloat
      , isDialog --> doCenterFloat
      ]
    ]
    where
        myFloats = ["MPlayer", "VLC media player" {- "Steam"-}]

myLayoutHook =
    workspaceDir "~" $ smartBorders $ avoidStruts $ layoutHook defaultConfig

myWorkspaces = ["1", "2", "3", "4",
                "5", "6", "7", "8",
                "9", "0"]

associate :: WorkspaceId -> String -> ManageHook
associate area wmClass = className =? wmClass --> doShift area

main = do 
    xmproc <- spawnPipe "xmobar"
    xmonad $ withUrgencyHook NoUrgencyHook myConf
          {
            logHook = dynamicLogWithPP $ xmobarPP
                              { 
                                ppOutput = hPutStrLn xmproc
                              , ppTitle = xmobarColor "green" "" . shorten 100 
                              , ppSep = " | "
                              , ppLayout = id
                              , ppUrgent = xmobarColor "grey" "red" . wrap "!" "!" . xmobarStrip
                              }
          }

myXPConfig = defaultXPConfig

myAddKeys =  
    [ ((0, 0x1008ff2e), spawn "amixer -q set Master 5-")
    , ((0, 0x1008ff12), spawn "amixer set Front toggle")
    , ((myModMask, 0x1008ff2e), spawn "amixer -q set Master 5+")
    , ((0, 0x1008ff10), spawn "mpc toggle")
    {-, ((0, 0x1008ff15), spawn "mpc stop")-}
    {-, ((0, 0x1008ff16), spawn "mpc prev")-}
    {-, ((0, 0x1008ff17), spawn "mpc next")-}

    , ((myModMask .|. shiftMask, xK_b), bringMenu)
    , ((myModMask .|. shiftMask, xK_n), refresh)

    , ((myModMask, xK_y), scratchpadSpawnActionTerminal "urxvt")

    , ((0, xK_Print), spawn "scrot")
    , ((shiftMask, xK_Print), spawn "sleep 0.2; scrot -s")

    , ((myModMask, xK_p), shellPrompt myXPConfig)
    , ((myModMask .|. shiftMask, xK_d), changeDir myXPConfig)
    ]

-- Colemak
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
 
    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
 
    -- launch dmenu
    --, ((modm,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
 
    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)
 
     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)
 
    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
 
    -- Resize viewed windows to the correct size
    , ((modm,               xK_k     ), refresh)
 
    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)
 
    -- Move focus to the next window
    , ((modm,               xK_n     ), windows W.focusDown)
 
    -- Move focus to the previous window
    , ((modm,               xK_e     ), windows W.focusUp  )
 
    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )
 
    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)
 
    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_n     ), windows W.swapDown  )
 
    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_e     ), windows W.swapUp    )
 
    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)
 
    -- Expand the master area
    , ((modm,               xK_i     ), sendMessage Expand)
 
    -- Push window back into tiling
    , ((modm,               xK_g     ), withFocused $ windows . W.sink)
 
    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
 
    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
 
    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    , ((modm              , xK_b     ), sendMessage ToggleStruts)
 
    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
 
    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++
 
    --
    -- mod-[1..9], Switch to workspace N
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
 
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_f, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

