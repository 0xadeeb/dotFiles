{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- Basic imports
import XMonad
import System.IO (hClose, hPutStr, hPutStrLn)
import System.Exit

-- Actions
import XMonad.Actions.FloatSnap

-- Data Structures
import Data.Char (isSpace, toUpper)
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
import XMonad.Hooks.RefocusLast -- (refocusLastLayoutHook, refocusLastWhen, isFloat)

-- Utils
import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import XMonad.Util.Loggers
import XMonad.Util.EZConfig (additionalKeysP, mkNamedKeymap)
import XMonad.Util.NamedActions
import XMonad.Util.Types ( Direction2D(U, D, L, R) )

-- Layouts
import XMonad.Layout.Grid
import XMonad.Layout.LayoutCombinators
import qualified XMonad.Layout.Magnifier as Mag
import XMonad.Layout.Simplest
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TrackFloating

-- Layout modifiers
import XMonad.Layout.Decoration
import XMonad.Layout.Groups
import XMonad.Layout.WindowNavigation
import XMonad.Layout.LayoutModifier
import XMonad.Layout.PerWorkspace
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.ShowWName
import XMonad.Layout.TabBarDecoration
-- import XMonad.Layout.IfMax
-- import XMonad.Layout.NoFrillsDecoration

-- For polybar
import qualified XMonad.DBus as D
import qualified DBus.Client as DC

myTerminal      = "kitty"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myBorderWidth   = 0

myModMask       = mod4Mask
myModMask'       = mod3Mask

-- setting colors for tabs layout and tabs sublayout.
base03  = "#002b36"
base02  = "#073642"
base01  = "#586e75"
base00  = "#657b83"
base0   = "#839496"
base1   = "#93a1a1"
base2   = "#eee8d5"
base3   = "#fdf6e3"
active      = "#bd93f9"
myTabTheme = def
    {
     activeColor           = active
    , inactiveColor         = base02
    , activeBorderColor     = active
    , inactiveBorderColor   = base02
    , activeTextColor       = base03
    , inactiveTextColor     = base00
    }

myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
  { swn_fade              = 1.0
  , swn_bgcolor           = "#1c1f24"
  , swn_color             = "#ffffff"
  }

myWorkspaces = ["Web", "Term", "Code", "Chat", "Vid" ] ++ map show [6..9] ++ ["Miscl"]

myNormalBorderColor  = "#ffffff"
myFocusedBorderColor = "#00ffff"

myEmacs = "emacsclient -c -a 'emacs' "
myXmobar = "~/.config/xmonad/xmobar.hs"
myBar = myXmobar
myWallpapers = "~/.wallpapers"
topbar = 15
myBrowserClass = "Brave-browser"

topBarTheme = def
    {
     inactiveBorderColor   = base03
    , inactiveColor         = base03
    , inactiveTextColor     = base03
    , activeBorderColor     = active
    , activeColor           = active
    , activeTextColor       = active
    , urgentBorderColor     = red
    , urgentTextColor       = yellow
    , decoHeight            = topbar
    }

subtitle' ::  String -> ((KeyMask, KeySym), NamedAction)
subtitle' x = ((0,0), NamedAction $ map toUpper
                      $ sep ++ "\n-- " ++ x ++ " --\n" ++ sep)
  where
    sep = replicate (6 + length x) '-'

showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
  h <- spawnPipe $ "yad --text-info --fontname=\"SauceCodePro Nerd Font Mono 12\" --fore=#e6f2ff back=#282c36 --center --geometry=1200x800 --title \"XMonad keybindings\""
  --hPutStr h (unlines $ showKm x) -- showKM adds ">>" before subtitles
  hPutStr h (unlines $ showKmSimple x) -- showKmSimple doesn't add ">>" to subtitles
  hClose h
  return ()

myKeys :: XConfig l0 -> [((KeyMask, KeySym), NamedAction)]
myKeys c =
    let subKeys str ks = subtitle' str : mkNamedKeymap c ks in
    subKeys "Custom keybindings"
    -- launch a terminal
    [ ("M-S-<Return>", addName "Spawns default terminal" $ spawn myTerminal)

    -- launchers
    , ("M-p", addName "Quick Launcher" $ spawn "rofi -show drun")
    , ("M-o", addName "Files Launcher" $ spawn "rofi -show filebrowser")

    -- close focused window
    , ("M-S-c", addName "Close focused window" $ kill)

     -- Rotate through the available layout algorithms
    , ("M-<Space>", addName "Rotate through the layouts" $ sendMessage NextLayout)

    -- Jump to layouts
    , ("M-t" , addName "Switch to tilling layout" $ sendMessage $ JumpToLayout "Tiled"     ) --Switch to the tiling layout
    , ("M-f" , addName "Switch to tabbed layout" $ sendMessage $ JumpToLayout "Tabbed"     ) --Switch to the full tabbed layout
    , ("M-g" ,  addName "Switch to grid layout" $ sendMessage $ JumpToLayout "Grid"     ) --Switch to the grid layout

    -- Resize viewed windows to the correct size
    , ("M-n", addName "Resize the window" $ refresh)

    -- Move focus to the next window
    , ("M-<Tab>", addName "Move focus to next window" $ windows W.focusDown)
    -- Move focus to the previous window
    , ("M-S-<Tab>", addName "Move focus to previous window" $ windows W.focusUp)

    -- Merging windows to subLayout groups
    , ("M-C-h", addName "Merge window with window in the left" $ sendMessage $ pullGroup L)
    , ("M-C-l", addName "Merge window with window in the right" $ sendMessage $ pullGroup R)
    , ("M-C-k", addName "Merge window with window above" $ sendMessage $ pullGroup U)
    , ("M-C-j", addName "Merge window with window below" $ sendMessage $ pullGroup D)

    , ("M-C-m", addName "Merge all the windows open in workspace" $ withFocused (sendMessage . MergeAll))
    , ("M-C-u", addName "UnMerge focused window from the group" $ withFocused (sendMessage . UnMerge))

    , ("M-C-.", addName "Switch focus to next window in group" $ onGroup W.focusUp')
    , ("M-C-,", addName "Switch focus to previous window in group" $ onGroup W.focusDown')

    -- Move focus to the next window
    , ("M-j", addName "Move focus to the next window" $ windows W.focusDown)

    -- Move focus to the previous window
    , ("M-k", addName "Move focus to previous window" $ windows W.focusUp  )

    -- Move focus to the master window
    -- , ("M-m", addName "Move focus to the master window" $ windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ("M-<Return>", addName "Swap focused and master window" $ windows W.swapMaster)

    -- Swap the focused window with the next window
    , ("M-S-j", addName "Swap the focused window with the next window" $ windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ("M-S-k", addName "Swap the focused window with the previous window" $ windows W.swapUp    )

    -- Shrink the master area
    , ("M-h", addName "Shrink the master area" $ sendMessage Shrink)

    -- Expand the master area
    , ("M-l", addName "Expand the master area" $ sendMessage Expand)

    -- Push window back into tiling
    , ("M-S-t", addName "Push window back into tiling" $ withFocused $ windows . W.sink)

    -- Increase the number of windows in the master area
    , ("M-,", addName "Increase the number of windows in the master area" $ sendMessage (IncMasterN 1))

    -- Decrease the number of windows in the master area
    , ("M-.", addName "Decrease the number of windows in the master area" $ sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    , ("M-S-b", addName "Toggle the status bar gap" $ sendMessage ToggleStruts)

    -- set wallpaper
    , ("M-b", addName "Change wallpaper" $ spawn $ "feh --randomize --bg-scale " ++ myWallpapers ++ "/*")

    -- Quit xmonad
    , ("M-S-q", addName "Quit xmonad" $ io (exitWith ExitSuccess))

    -- Restart xmonad
    , ("M-q", addName "Restart xmonad" $ spawn "xmonad --recompile && ( xmonad --restart )")

    -- Open emacs
    , ("M-e e", addName "Open emacs" $ spawn $ myEmacs ++ "-e '(dashboard-refresh-buffer)'")

    -- Restart emacs server
    , ("M-e r", addName "Restart emacs server" $ spawn  "~/.local/bin/startEmacs.sh")

    -- Lock screen
    , ("M-S-l", addName "Lock screen" $ spawn  "betterlockscreen -l")

    -- Power menu
    , ("M-x", addName "Spawn power menu" $ spawn "oblogout")

    -- No borders
    --, ("M-S-n" SendMessage )

    -- Move to miscl workspace
    , ("M-m", addName "Switch to last workspace" $ windows $ W.greedyView $ last myWorkspaces)
    , ("M-S-m", addName "Move focused window to last workspace" $ windows $ W.shift $ last myWorkspaces)

    ]

    ^++^ subKeys "Mutimedia keys"
    [
    -- Increase Brightness
    ("<XF86MonBrightnessUp>", addName "Increase Brightness" $ spawn "~/.config/dunst/brightness/brightnessControl.sh up")

    -- Decrease Brightness
    , ("<XF86MonBrightnessDown>", addName "Decrease Brightness" $ spawn "~/.config/dunst/brightness/brightnessControl.sh down")

    -- Increase volume
    , ("<XF86AudioRaiseVolume>", addName "Increase volume" $ spawn "~/.config/dunst/volume/volume.sh up")

    -- Decrease volume
    , ("<XF86AudioLowerVolume>", addName "Decrease volume" $ spawn "~/.config/dunst/volume/volume.sh down")

    -- Mute and unmute
    , ("<XF86AudioMute>", addName "Mute and unmute" $ spawn "~/.config/dunst/volume/volume.sh toggle")

    -- Screenshot
    , ("<Print>",  addName "Take screenshot" $spawn "~/.local/bin/screenShot.sh")
    ]

    ^++^ subKeys "Workspace related keys"
    [("M-" ++ e ++ (show i), addName (m ++ (show i)) $ windows $ f $ myWorkspaces !! (i - 1))
        | i <- [1..9]
        , (f, e, m) <- [(W.greedyView, "", "Switch to workspace "), (W.shift, "S-", "Move focused window to workspace ")]]

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

mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw False (Border i (2 * i) i i) True (Border 0 0 i i) True

myTabbedSpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
myTabbedSpacing i = spacingRaw False (Border (2 * i) (2 * i) (2 * i) (2 * i)) True (Border 0 0 0 0) True

newtype SmartBarDeco a = SmartBarDeco Direction2D
  deriving (Eq, Show, Read)

instance Eq a => DecorationStyle SmartBarDeco a where
  describeDeco _ = "SmartDeco"

  shrink (SmartBarDeco direction) = shrinkWinForDeco direction
   where
    shrinkWinForDeco :: Direction2D -> Rectangle -> Rectangle -> Rectangle
    shrinkWinForDeco U (Rectangle _ _ _ dh) (Rectangle x y w h) = Rectangle x (y + fi dh) w (h - fi dh)
    shrinkWinForDeco D (Rectangle _ _ _ dh) (Rectangle x y w h) = Rectangle x y w (h - fi dh)
    shrinkWinForDeco L (Rectangle _ _ dw _) (Rectangle x y w h) = Rectangle (x + fi dw) y (w - fi dw) h
    shrinkWinForDeco R (Rectangle _ _ dw _) (Rectangle x y w h) = Rectangle x y (w - fi dw) h

  pureDecoration (SmartBarDeco direction) decoWidth decoHeight _ s windowRects currentWin@(_win, Rectangle x y w h)
    | (isInStack s _win) && (decoHeight < h) && (length windowRects > 1) = Just smartBarBar
    | otherwise = Nothing
   where
    smartBarBar = case direction of
      U -> Rectangle x y w decoHeight
      D -> Rectangle x (y + fi (h - decoHeight)) w decoHeight
      L -> Rectangle x y decoWidth h
      R -> Rectangle (x + fi (w - decoWidth)) y decoWidth h

smartBarDeco
  :: Eq a
  => Direction2D
  -> Theme
  -> l a
  -> ModifiedLayout (Decoration SmartBarDeco DefaultShrinker) l a
smartBarDeco direction theme =
  decoration shrinkText theme $ SmartBarDeco direction

myLayout
  = refocusLastLayoutHook . trackFloating
    $ tiled
        ||| grid
        ||| threeCol
        ||| floats
        ||| magnifiedTiled
        ||| mirror
        ||| full
        ||| tabs
        ||| spiral1

  where

    tiled             = renamed [Replace "Tiled"]
                            $ avoidStruts
                            $ addTopBar
                            $ windowNavigation
                            $ addTabs shrinkText myTabTheme
                            $ subLayout [] Simplest
                            $ mySpacing gap
                            $ Tall nmaster delta ratio

    threeCol          = renamed [Replace "Three Column"]
                            $ avoidStruts
                            $ addTopBar
                            $ windowNavigation
                            $ addTabs shrinkText myTabTheme
                            $ subLayout [] Simplest
                            $ mySpacing gap
                            $ ThreeColMid nmaster delta ratio

    magnifiedTiled    = renamed [Replace "Magnified"]
                            $ avoidStruts
                            $ addTopBar
                            $ windowNavigation
                            $ addTabs shrinkText myTabTheme
                            $ subLayout [] Simplest
                            $ mySpacing gap
                            $ Mag.magnifiercz' 1.1
                            $ Tall nmaster delta ratio

    grid              = renamed [Replace "Grid"]
                            $ avoidStruts
                            $ addTopBar
                            $ windowNavigation
                            $ addTabs shrinkText myTabTheme
                            $ subLayout [] Simplest
                            $ mySpacing gap
                            $ limitWindows 12
                            $ Grid

    full              = renamed [Replace "Full"]
                            $ avoidStruts
                            $ mySpacing gap
                            $ smartBorders
                            $ noBorders
                            $ Full

    mirror            = renamed [Replace "Mirror Tiled"]
                            $ avoidStruts
                            $ addTopBar
                            $ windowNavigation
                            $ addTabs shrinkText myTabTheme
                            $ subLayout [] Simplest
                            $ mySpacing gap
                            $ Mirror
                            $ Tall nmaster delta ratio

    floats             = renamed [Replace "Float"]
                            $ avoidStruts
                            $ mySpacing gap
                            $ smartBorders
                            $ limitWindows 20
                            $ simplestFloat

    tabs              = renamed [Replace "Tabbed"]
                            $ avoidStruts
                            $ myTabbedSpacing gap
                            $ tabbed shrinkText myTabTheme

    spiral1            = renamed [Replace "Spiral"]
                            $ avoidStruts
                            $ addTopBar
                            $ windowNavigation
                            $ addTabs shrinkText myTabTheme
                            $ subLayout [] Simplest
                            $ mySpacing gap
                            $ limitWindows 12
                            $ spiral (6/7)

    addTopBar = smartBarDeco U topBarTheme
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio   = 1/2
    -- Percent of screen to increment by when resizing panes
    delta   = 3/100
    -- Border space
    gap = 10

manageWorkspace
  :: (WorkspaceId -> Bool)
  -> ManageHook
  -> ManageHook
  -> ManageHook
manageWorkspace p h1 h2 = do
    i <- liftX $ gets $ W.currentTag . windowset
    if p i
       then h1
    else h2

myManageHook =
    manageSpawn
    <+> insertPosition Master Newer
      where
        manageSpawn = composeOne
          [ className =? "mpv"            -?> doFloat
          , className =? "Oblogout"       -?> doFloat
          , className =? "Yad"       -?> doFloat
          , className =? "QjackCtl"       -?> doCenterFloat
          , isRole =? "pop-up" -?> doCenterFloat
          , isDialog                      -?> doCenterFloat
          , resource  =? "desktop_window" -?> doIgnore
          , isDialog  -?> doCenterFloat
          , isBrowserDialog -?> forceCenterFloat
          , isRole =? gtkFile  -?> forceCenterFloat
          , isInProperty "_NET_WM_WINDOW_TYPE"
                         "_NET_WM_WINDOW_TYPE_SPLASH" -?> doCenterFloat
          , className =? myBrowserClass  -?> doShift $ myWorkspaces !! 0
          , className =? "Emacs"          -?> doShift $ myWorkspaces !! 2
          ]
        isBrowserDialog = isDialog <&&> className =? myBrowserClass
        gtkFile = "GtkFileChooserDialog"
        isRole = stringProperty "WM_WINDOW_ROLE"

forceCenterFloat :: ManageHook
forceCenterFloat = doFloatDep move
  where
    move :: W.RationalRect -> W.RationalRect
    move _ = W.RationalRect x y w h

    w, h, x, y :: Rational
    w = 1/3
    h = 1/2
    x = (1-w)/2
    y = (1-h)/2

myEventHook =
  mconcat
  [
   refocusLastWhen (refocusingIsActive <||> isFloat),
   swallowEventHook (className =? "kitty" <||> className =? "Termite") (className /=? "Zenity" <&&> className /=? "Yad")
  ]

myLogHook :: X ()
myLogHook = refocusLastLogHook <> fadeInactiveLogHook fadeAmount
            where fadeAmount = 0.9

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
    , ppOrder           = \[_,l,_] -> [l]
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
  spawnOnce "nm-applet --sm-disable &"
  spawnOnce "blueman-applet &"
  spawnOnce "flameshot &"
  spawnOnce "[[ -s ~/.Xmodmap ]] && xmodmap ~/.Xmodmap"
  spawnOnce "/usr/bin/lxqt-policykit-agent &"
  spawnOnce "libinput-gestures-setup start"
  spawnOnce "xfce4-power-manager &"
  spawnOnce "picom &"
  spawnOnce "parcellite &"
  spawnOnce "alttab -fg \"#d58681\" -bg \"#4a4a4a\" -frame \"#eb564d\" -t 128x150 -i 127x64 -w 1 &"
  spawnOnce "~/.config/polybar/launch.sh --forest"
  spawnOnce "~/.config/conky/conky-startup.sh"
  spawnOnce "~/.local/bin/startEmacs.sh"

main = do
  -- Connect to DBus
  dbus <- D.connect
  -- Request access (needed when sending messages)
  D.requestAccess dbus

  xmonad
    $ addDescrKeys' ((mod4Mask .|. shiftMask, xK_slash), showKeybindings) myKeys
    $ ewmhFullscreen
    $ ewmh
    -- $ withEasySB (statusBarProp ("xmobar " ++ myBar) (pure myXmobarPP)) defToggleStrutsKey
    $ docks
    $ defaults dbus

defaults dbus = def {
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
        layoutHook         = showWName' myShowWNameTheme $ myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = dynamicLogWithPP (myLogHookPP dbus) <+> myLogHook,
        startupHook        = myStartupHook
    }
