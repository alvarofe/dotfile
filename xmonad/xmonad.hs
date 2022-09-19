--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import XMonad
import Data.Monoid
import System.Exit

-- Utils
import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import GHC.IO.Handle.Types (Handle)
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Man
import XMonad.Util.NamedScratchpad

import Data.Maybe (fromJust)

-- Layout
import XMonad.Layout.IndependentScreens
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Simplest
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Renamed
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Fullscreen
import XMonad.Layout.MultiToggle
import XMonad.Layout.Named
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.NoBorders
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ResizableTile
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Grid
import XMonad.Layout.Gaps
import XMonad.Layout.Hidden
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.BoringWindows as B
import qualified XMonad.Layout.Magnifier as MAG

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.FadeInactive (fadeInactiveCurrentWSLogHook)

--Actions
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.Promote
import XMonad.Actions.CycleWS

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "kitty"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

ctrlMask :: KeyMask
ctrlMask = controlMask

-- Width of the window border in pixels.
--
myBorderWidth   = 1

myStatusBar :: String
myStatusBar = "xmobar"

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod1Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces = withScreens 2 ["1","2","3","4","5","6","7","8","9"]


myFont = "xft:Iosevka Term SS02:size=10:antialias=true:hinting=true"

-- colors
red         = "#AB4642"
orange      = "#DC9656"
yellow      = "#F7CA88"
white       = "#FFFFFF"
green       = "#A1B56C"
turquoise   = "#86C1B9"
blue        = "#005577"
purple      = "#BA8BAF"
brown       = "#A16946"
bg_color    = "#181818"
fg_dark     = "#333333"

-- this is a "fake title" used as a highlight bar in lieu of full borders
-- (I find this a cleaner and less visually intrusive solution)

myDecoTheme = def { fontName            = myFont
                  , activeColor         = red
                  , inactiveColor       = fg_dark
                  , activeBorderColor   = red
                  , inactiveBorderColor = fg_dark
                  , activeTextColor     = red
                  , inactiveTextColor   = fg_dark
                  , decoHeight          = 5
                  }

myTabTheme = def { fontName             = myFont
                 , activeColor          = red
                 , inactiveColor        = fg_dark
                 , activeBorderColor    = red
                 , inactiveBorderColor  = fg_dark
                 , activeTextColor      = fg_dark
                 , inactiveTextColor    = red
                 , decoHeight           = 14
                 }

myXPConfig = def
  { position          = Top
  , alwaysHighlight   = True
  , promptBorderWidth = 0
  , font              = "xft:monospace:size=9"
  }


myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm ]
        where
            spawnTerm  = myTerminal ++ " --title scratchpad"
            findTerm   = title =? "scratchpad"
            manageTerm = customFloating $ W.RationalRect l t w h
                       where
                         h = 0.89
                         w = 0.90
                         t = 0.95 -h
                         l = 0.95 -w

barFull = avoidStruts $ Simplest

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#ff0000"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,               xK_p     ), spawn "dmenu_run")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    , ((modm .|. shiftMask, xK_f ), sendMessage (T.Toggle "full"))
    , ((modm,               xK_b ), sendMessage ToggleStruts)
    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_Up ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_Down), sendMessage (IncMasterN (-1)))

    , ((modm              , xK_b     ), sendMessage ToggleStruts)
    , ((modm              , xK_r     ), spawn "redshift -O 2400")
    , ((modm .|. shiftMask, xK_r     ), spawn "redshift -x")
    , ((modm              , xK_F1    ), manPrompt myXPConfig)
    , ((modm              , xK_F12   ), spawn "amixer -q sset Master 3%+")
    , ((modm              , xK_F11   ), spawn "amixer -q sset Master 3%-")
    , ((modm              , xK_F10   ), spawn "amixer -q sset Master toggle")
    , ((modm              , xK_F7    ), spawn "playerctl play-pause")
    , ((modm              , xK_F8    ), spawn "playerctl previous")
    , ((modm              , xK_F9    ), spawn "playerctl next")
    , ((modm              , xK_Tab   ), toggleWS)
    , ((modm .|. shiftMask, xK_p     ), spawn "screenshot -sc")
    , ((modm .|. shiftMask, xK_e     ), spawn "emacs")
    , ((modm .|. shiftMask, xK_l     ), spawn "slock")
    , ((modm .|. shiftMask, xK_z     ), spawn "slock systemctl suspend -i")

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")

    , ((modm .|. shiftMask, xK_t     ), namedScratchpadAction myScratchPads "terminal")
    , ((modm              , xK_backslash), withFocused hideWindow)
    , ((modm .|. shiftMask, xK_backslash), popOldestHiddenWindow)
    , ((modm .|. ctrlMask , xK_Left     ), sendMessage $ pullGroup L)
    , ((modm .|. ctrlMask , xK_Right    ), sendMessage $ pullGroup R)
    , ((modm .|. ctrlMask , xK_Up       ), sendMessage $ pullGroup U)
    , ((modm .|. ctrlMask , xK_Down     ), sendMessage $ pullGroup D)
    , ((modm .|. ctrlMask , xK_m     ), withFocused $ (sendMessage . MergeAll))
    , ((modm .|. ctrlMask , xK_slash ), withFocused $ (sendMessage . UnMergeAll))
    , ((modm .|. ctrlMask , xK_j     ), onGroup W.focusUp')
    , ((modm .|. ctrlMask , xK_k     ), onGroup W.focusDown')
    , ((modm .|. ctrlMask , xK_u     ), withFocused $ (sendMessage . UnMerge))

    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ onCurrentScreen f i)
        | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_comma, xK_period] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

myLayout = avoidStruts
         $ hiddenWindows 
         $ noBorders
         $ windowNavigation
         $ B.boringWindows
         $ toggleLayouts ( renamed [Replace "full"]
                         $ Full
                         )
         $ renamed [CutWordsLeft 5]
         $ smartSpacingWithEdge 1
         $ noFrillsDeco shrinkText myDecoTheme
         $ addTabs shrinkText myTabTheme
         $ subLayout [] (Simplest)
         $ MAG.magnifierOff
         (   ( renamed [Replace "m/s"]
             $ ResizableTall 1 (3/100) (1/2) []
             )
         -- ||| ( renamed [Replace "BSP"]
             -- $ emptyBSP
             -- )
         ||| ( renamed [Replace "3col"]
             $ ThreeColMid 1 (3/100) (1/2)
             )
         ||| ( renamed [Replace "mirror m/s"]
             $ Mirror (ResizableTall 1 (3/100) (1/2) [])
             )
         ||| ( renamed [Replace "grid"]
             $ Grid
             )
         )

------------------------------------------------------------------------
-- Window rules:

myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    ] <+> namedScratchpadManageHook myScratchPads

------------------------------------------------------------------------
myEventHook = mempty

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
        spawnOnce "source ~/.xinitrc"
        spawnOnce "nm-applet"
        spawnOnce "blueman-applet"

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
xmobarCommand :: ScreenId -> String
xmobarCommand (S screenNumber) =
  unwords
    [ myStatusBar,
      "-x",
      show screenNumber,
      "~/.config/xmobar/xmobar.config"
    ]

myBarPrettyPrinter :: Handle -> ScreenId -> PP
myBarPrettyPrinter handle screenNumber =
  namedScratchpadFilterOutWorkspacePP $ marshallPP
    screenNumber
    def
      {
        ppVisible = xmobarColor yellow "",
        -- ppUrgent = showMyThemeColor color9,
        ppUrgent = xmobarColor red "" . wrap "!" "!",
        -- ppOrder = \(wss : layout : _ : _) -> [wss, layout],
        ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t],
        ppOutput = hPutStrLn handle,
        -- ppTitle = xmobarColor barTitleColor "" . shorten 39,
        ppCurrent = xmobarColor white blue . pad,
        ppHidden = xmobarColor white "",
        --ppHiddenNoWindows = xmobarColor red "",
        ppSep = "  ",
        ppLayout = xmobarColor yellow ""
      }

main :: IO ()
main = do
        screenNumber <- countScreens
        handles <- mapM (spawnPipe . xmobarCommand) [0 .. screenNumber - 1]
        xmonad $ docks $ def {
                terminal           = myTerminal,
                focusFollowsMouse  = myFocusFollowsMouse,
                clickJustFocuses   = myClickJustFocuses,
                borderWidth        = myBorderWidth,
                modMask            = myModMask,
                workspaces         = myWorkspaces,
                normalBorderColor  = myNormalBorderColor,
                focusedBorderColor = myFocusedBorderColor,

                -- key bindings
                keys               = myKeys,
                mouseBindings      = myMouseBindings,

                -- hooks, layouts
                layoutHook         = myLayout,
                manageHook         = myManageHook,
                handleEventHook    = myEventHook,
                startupHook        = myStartupHook,
                logHook = fadeInactiveCurrentWSLogHook 0.8 <+>  mapM_ dynamicLogWithPP (zipWith myBarPrettyPrinter handles [0 .. screenNumber])
    }
