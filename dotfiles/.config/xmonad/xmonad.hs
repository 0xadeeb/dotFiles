-- Basic imports
import XMonad -- hiding ( (|||) )
import System.Exit

-- Actions
import XMonad.Actions.FloatSnap

-- Data Structures
import Data.Monoid
import qualified Data.Map as M

-- Data types
import qualified XMonad.StackSet as W

-- Hooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.WindowSwallowing

-- Utils
import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import XMonad.Util.Loggers
import XMonad.Util.EZConfig (additionalKeysP)

-- Layouts
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Grid -- hiding ( (|||) )
import XMonad.Layout.LayoutCombinators

-- Layout modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Spacing
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Renamed
import XMonad.Layout.NoBorders
import qualified XMonad.Layout.Magnifier as Mag

-- For polybar
import qualified XMonad.DBus as D
import qualified DBus.Client as DC

myTerminal      = "kitty"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myBorderWidth   = 0

myModMask       = mod4Mask
myModMask'       = mod3Mask

-- A tagging example:
--  workspaces = ["web", "irc", "code" ] ++ map show [4..9]
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

myNormalBorderColor  = "#ffffff"
myFocusedBorderColor = "#00ffff"

myEmacs = "emacsclient -c -a 'emacs' "
myXmobar = "~/.config/xmonad/xmobar.hs"
myBar = myXmobar
myWallpapers = "~/.wallpapers"

myKeys :: [(String , X ())]
myKeys =
    -- launch a terminal
    [ ("M-S-<Return>", spawn myTerminal)

    -- launch dmenu
    , ("M-p", spawn "rofi -show drun")
    , ("M-o", spawn "rofi -show filebrowser")

    -- launch gmrun
    , ("M-S-p", spawn "gmrun")

    -- close focused window
    , ("M-S-c", kill)

     -- Rotate through the available layout algorithms
    , ("M-<Space>", sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    -- , ("M-S-<Space>", setLayout $ XMonad.layoutHook XConfig)

    -- Jump to layouts
    , ("M-f" , sendMessage $ JumpToLayout "Full"     ) --Switch to the full layout
    , ("M-g" , sendMessage $ JumpToLayout "Grid"     ) --Switch to the grid layout

    -- Resize viewed windows to the correct size
    , ("M-n", refresh)

    -- Move focus to the next window
    , ("M-<Tab>", windows W.focusDown)

    -- Move focus to the next window
    , ("M-j", windows W.focusDown)

    -- Move focus to the previous window
    , ("M-k", windows W.focusUp  )

    -- Move focus to the master window
    , ("M-m", windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ("M-<Return>", windows W.swapMaster)

    -- Swap the focused window with the next window
    , ("M-S-j", windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ("M-S-k", windows W.swapUp    )

    -- Shrink the master area
    , ("M-h", sendMessage Shrink)

    -- Expand the master area
    , ("M-l", sendMessage Expand)

    -- Push window back into tiling
    , ("M-t", withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ("M-,", sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ("M-.", sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.

    , ("M-S-b", sendMessage ToggleStruts)

    -- Quit xmonad
    , ("M-S-q", io (exitWith ExitSuccess))

    -- Restart xmonad
    , ("M-q", spawn "xmonad --recompile && ( xmonad --restart )")

    -- Open emacs
    , ("M-e e", spawn $ myEmacs ++ "-e '(dashboard-refresh-buffer)'")

    -- Lock screen
    , ("M-S-l", spawn  "betterlockscreen -l")

    -- Power menu
    , ("M-x", spawn  "oblogout")

    -- Increase Brightness
    , ("<XF86MonBrightnessUp>", spawn "~/.config/dunst/brightness/brightnessControl.sh up")

    -- Decrease Brightness
    , ("<XF86MonBrightnessDown>", spawn "~/.config/dunst/brightness/brightnessControl.sh down")

    -- Increase volume
    , ("<XF86AudioRaiseVolume>", spawn "~/.config/dunst/volume/volume.sh up")

    -- Decrease volume
    , ("<XF86AudioLowerVolume>", spawn "~/.config/dunst/volume/volume.sh down")

    -- Mute and unmute
    , ("<XF86AudioMute>", spawn "~/.config/dunst/volume/volume.sh toggle")

    -- No borders
    --, ("M-S-n" SendMessage )
    ]

    ++
    [("M-" ++ m ++ [i], windows $ f [i])
        | i <- "123456789"
        , (f, m) <- [(W.greedyView, ""), (W.shift, "S-")]]

myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList

    -- mod-button1, Set the window to floating mode and move by dragging
    -- [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
    --                                    >> windows W.shiftMaster))

    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                       >> afterDrag (snapMagicResize [L,R,U,D] (Just 50) (Just 50) w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster)

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

myLayout
  = renamed [CutWordsLeft 1] $ mySpacing 8 $ avoidStruts $ smartBorders $ tiled
    ||| grid
    ||| floats
    ||| magnifiedTiled
    ||| Mirror tiled
    ||| noBorders Full
  where
    magnifiedTiled = renamed [Replace "Magnified"] $ Mag.magnifiercz' 1.1 basic
    grid = renamed [Replace "Grid"] $ limitWindows 12 $ Grid
    tiled = renamed [Replace "Tiled"] basic
    floats = renamed [Replace "Floats"] $ limitWindows 20 simplestFloat

    -- default tiling algorithm partitions the screen into two panes
    basic = Tall nmaster delta ratio
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio   = 1/2
    -- Percent of screen to increment by when resizing panes
    delta   = 3/100
    -- Border space
    i = 8

myManageHook = composeAll
    [
      className =? "MPlayer"        --> doFloat
    , className =? "Oblogout"        --> doFloat
    -- , className =? "Gimp"           --> doFloat
    , isDialog                      --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    ] <+> insertPosition Master Newer

myEventHook = swallowEventHook (className =? "kitty"  <||> className =? "Termite") (return True)

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
            where fadeAmount = 0.85

blue, lowWhite, magenta, red, white, yellow :: String
magenta  = "#ff79c6"
blue     = "#bd93f9"
white    = "#f8f8f2"
yellow   = "#f1fa8c"
red      = "#ff5555"
lowWhite = "#bbbbbb"

myLogHookPP :: DC.Client -> PP
myLogHookPP dbus = def
    {
     ppOutput = D.send dbus
    , ppCurrent = wrap ("%{F" ++ blue ++ "} ") " %{F-}"
    , ppVisible = wrap ("%{F" ++ blue ++ "} ") " %{F-}"
    , ppUrgent = wrap ("%{F" ++ red ++ "} ") " %{F-}"
    , ppTitle = wrap ("%{F" ++ lowWhite ++ "} ") " %{F-}"
    , ppOrder           = \[_ , l, _] -> [l]
    , ppSep  =  "•"
    }

myAddSpaces :: Int -> String -> String
myAddSpaces len str = sstr ++ replicate (len - length sstr) ' '
  where
    sstr = shorten len str

myStartupHook :: X ()
myStartupHook = do
  -- spawn $ "wal -i " ++ myWallpapers -- pywal sets random wallpaper
  spawn $ "feh --randomize --bg-scale " ++ myWallpapers ++ "/*"  -- set wallpaper
  spawnOnce "xsetroot -cursor_name left_ptr"
  -- spawnOnce "sleep 1; trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 2 --transparent true --tint 0x5f5f5f --height 30 &"
  -- spawnOnce "xscreensaver -no-splash &"
  spawnOnce "nm-applet --sm-disable &"
  spawnOnce "~/.config/polybar/launch.sh --forest"
  spawnOnce "[[ -s ~/.Xmodmap ]] && xmodmap ~/.Xmodmap"
  spawnOnce "lxsession &"
  spawnOnce "xfce4-power-manager &"
  spawnOnce "picom &"
  spawnOnce "alttab -fg \"#d58681\" -bg \"#4a4a4a\" -frame \"#eb564d\" -t 128x150 -i 127x64 -w 1"
  spawnOnce "~/.config/conky/conky-startup.sh"
  spawnOnce "/usr/bin/emacs --daemon"

xblue, xlowWhite, xmagenta, xred, xwhite, xyellow :: String -> String
xmagenta  = xmobarColor magenta  ""
xblue     = xmobarColor blue     ""
xwhite    = xmobarColor white    ""
xyellow   = xmobarColor yellow   ""
xred      = xmobarColor red      ""
xlowWhite = xmobarColor lowWhite ""

main = do
  -- Connect to DBus
  dbus <- D.connect
  -- Request access (needed when sending messages)
  D.requestAccess dbus

  xmonad
    $ ewmhFullscreen
    $ ewmh
    -- $ withEasySB (statusBarProp ("xmobar " ++ myBar) (pure myXmobarPP)) defToggleStrutsKey
    $ docks defaults {logHook = dynamicLogWithPP (myLogHookPP dbus) <+> myLogHook}

defaults = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        -- numlockMask deprecated in 0.9.1
        -- numlockMask        = myNumlockMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
      --  keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        -- logHook            = myLogHook,
        startupHook        = myStartupHook
    } `additionalKeysP` myKeys
