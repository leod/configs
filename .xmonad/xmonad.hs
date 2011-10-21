import System.IO (hPutStrLn)

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Layout.NoBorders

main = do
    xmobarproc <- spawnPipe "xmobar"

    xmonad $ defaultConfig
        { terminal = "urxvt"
        , modMask = mod4Mask -- Windows/Super key

        -- Add support for docks and status bars
        , manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = smartBorders $ avoidStruts $ layoutHook defaultConfig

        , logHook = dynamicLogWithPP xmobarPP
            { ppOutput = hPutStrLn xmobarproc
            , ppTitle = xmobarColor "green" "" . shorten 50
            }
        }
