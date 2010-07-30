import Data.List
import Data.Ratio
import qualified Data.Map as M
import System.IO
import System.Exit
import Control.Monad ((<=<))

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
import XMonad.Layout.IM
import XMonad.Layout.Reflect
import XMonad.Actions.MouseGestures
import XMonad.Actions.CycleWS
import XMonad.Actions.SpawnOn
import XMonad.Actions.WindowBringer
import XMonad.Actions.TopicSpace
import XMonad.Actions.GridSelect
import XMonad.Util.Scratchpad
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Workspace

myConf sp = defaultConfig
            {
              terminal = "urxvt"
            , focusedBorderColor = "#aa0000"
            , normalBorderColor = "#333333"
            , workspaces = myTopics
            , keys = myKeys
            , manageHook = manageSpawn sp <+> manageDocks <+> manageHook defaultConfig <+> composeAll myManageHook
            , layoutHook = myLayoutHook
            , modMask = myModMask
            }

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
    onWorkspace "gimp" gimp $
    workspaceDir "~" $
    smartBorders $ avoidStruts $ layoutHook defaultConfig

    where
        gimp = withIM (0.11) (Role "gimp-toolbox") $
               reflectHoriz $
               withIM (0.15) (Role "gimp-dock") Full

myTopics = [ "web", "im", "dev", "admin"
           , "movie", "music", "mail", "dash"
           , "reading", "school", "defend"
           , "gaming", "gimp", "torrent" 
           ]

myTopicConfig sp = TopicConfig
    { topicDirs = M.fromList $
        [ ("torrent", "/mnt/media/dl"),
          ("music", "~/share/music"),
          ("dev", "~/dev"),
          ("movie", "/mnt/media/tv"),
          ("school", "~/school"),
          ("defend", "~/dev/defend"),
          ("gaming", "~/.wine/drive_c/Program\\ Files")
        ]
    , defaultTopicAction = const $ spawnShell sp
    , defaultTopic = "web"
    , maxTopicHistory = 10
    , topicActions = M.fromList $
        [ ("web", spawnOn sp "web" "firefox")
        , ("music", spawnOn sp "music" "sonata")
        , ("mail", spawnOn sp "mail" "thunderbird")
        , ("im", spawnOn sp "im" "pidgin" >> spawnOn sp "im" "urxvt -e ssh orion")
        , ("pdf", spawnOn sp "pdf" "okular")
        , ("gimp", spawnOn sp "gimp" "gimp")
       ]
    }

spawnShell sp = currentTopicDir (myTopicConfig sp) >>= spawnShellIn sp

spawnShellIn sp dir= do
    t <- asks (terminal . config)
    spawnHere sp $ "cd " ++ dir ++ " && " ++ t

associate :: WorkspaceId -> String -> ManageHook
associate area wmClass = className =? wmClass --> doShift area

main = do 
    xmproc <- spawnPipe "xmobar"
    sp <- mkSpawner
    xmonad $ withUrgencyHook NoUrgencyHook (myConf sp)
        { logHook = {-io . hPutStrLn xmproc =<< (XMonad.Actions.TopicSpace.pprWindowSet (myTopicConfig sp)-} dynamicLogWithPP
              xmobarPP
              { ppOutput = hPutStrLn xmproc
              , ppTitle = xmobarColor "green" "" . shorten 100 
              , ppSep = " | "
              , ppLayout = id
              , ppUrgent = xmobarColor "grey" "red" . wrap "!" "!" . xmobarStrip
              }
        } `additionalKeys` myAddKeys sp

myXPConfig = defaultXPConfig

gsConfig = defaultGSConfig { gs_navigate = hnei `M.union` gs_navigate (defaultGSConfig `asTypeOf` gsConfig) }
    where
        hnei = M.insert (0, xK_space) (const (0, 0)) $ M.map (\(x, y) (a, b) -> (x + a, y + b)) $ M.fromList
               [ ((0, xK_h), (-1, 0))
               , ((0, xK_n), ( 0, 1))
               , ((0, xK_i), ( 1, 0))
               , ((0, xK_e), ( 0,-1))
               ]

wsgrid = gridselect gsConfig <=< asks $ map (\x -> (x,x)) . workspaces . config

goto sp = switchTopic (myTopicConfig sp)

--promptedGoto sp = wsgrid >>= flip whenJust (goto sp)
promptedGoto sp = workspacePrompt myXPConfig $ goto sp

myAddKeys sp =  
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
    
    -- Topics
    , ((myModMask, xK_t), promptedGoto sp)
    , ((myModMask .|. shiftMask, xK_t), workspacePrompt myXPConfig $ windows . W.shift)
    , ((myModMask, xK_a), currentTopicAction (myTopicConfig sp))
    ]
    {-++
    [ ((myModMask, k), switchNthLastFocused (myTopicConfig sp) i)
    | (i, k) <- zip [1..] [xK_1 .. xK_9] ]-}
    {-++
    [ ((myModMask .|. m, k), f topic)
    | (topic, k) <- [("web", xK_l), ("im", xK_u), ("dev", xK_y), ("music", xK_semicolon)]
    , (f, m) <- [(switchTopic (myTopicConfig sp), 0), (windows . W.shift, shiftMask)]] -}

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

