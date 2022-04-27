import Xmobar
import Colors

-- Example user-defined plugin

-- data HelloWorld = HelloWorld
--     deriving (Read, Show)

-- instance Exec HelloWorld where
--     alias HelloWorld = "hw"
--     run   HelloWorld = return "<fc=" ++ color01 ++ ">Hello World!!</fc>"

-- Configuration, using predefined monitors as well as our HelloWorld
-- plugin:

config :: Config
config = defaultConfig {

   -- appearance
   font =         "xft:JetBrains Mono:size=15:bold:antialias=true"
   , additionalFonts = [ --"xft:Mononoki:pixelsize=11:antialias=true:hinting=true"
                           "xft:Font Awesome 6 Free Solid:pixelsize=12"
                           , "xft:Font Awesome 6 Brands:pixelsize=12"
                           ]

   , bgColor =      background
   , fgColor =      foreground
   , position =     Static { xpos = 18 , ypos = 10, width = 1880, height = 35}
   -- , position = TopW C 98
   -- , border =       TopB 10
   , borderColor =  "#646464"
   , iconRoot     = "/home/adeeb/.xmonad/xpmImages/" -- default: "."

   -- layout
   , sepChar =  "%"   -- delineator between plugin names and straight text
   , alignSep = "}{"  -- separator between left-right alignment

   -- general behavior
   , lowerOnStart =     True    -- send to bottom of window stack on start
   , hideOnStart =      False   -- start with window unmapped (hidden)
   , allDesktops =      True    -- show on all desktops
   , overrideRedirect = True    -- set the Override Redirect flag (Xlib)
   , pickBroadest =     False   -- choose widest display (multi-monitor)
   , persistent =       True    -- enable/disable hiding (True = disabled)

   -- plugins
   --   Numbers can be automatically colored according to their value. xmobar
   --   decides color based on a three-tier/two-cutoff system, controlled by
   --   command options:
   --     --Low sets the low cutoff
   --     --High sets the high cutoff
   --
   --     --low sets the color below --Low cutoff
   --     --normal sets the color between --Low and --High cutoffs
   --     --High sets the color above --High cutoff
   --
   --   The --template option controls how the plugin is displayed. Text
   --   color can be set by enclosing in <fc></fc> tags. For more details
   --   see http://projects.haskell.org/xmobar/#system-monitor-plugins.
   , commands =
        [
        -- cpu activity monitor
        Run $ MultiCpu       [ "--template" , "<box type=Bottom width=4 color=" ++ color1 ++ "> <fn=1>\xf108</fn> <total>% </box>"
                             , "--Low"      , "50"         -- units: %
                             , "--High"     , "85"         -- units: %
                             , "--low"      , "green"
                             , "--normal"   , "orange"
                             , "--high"     , "red"
                             ] 10
        -- Volume
        , Run $ Volume "default" "Master"
                        [ "--template", "<box type=Bottom width=4 color=" ++ color2 ++ "> <fn=1>\xf028</fn> <volumestatus> </box>"
                        , "--suffix"  , "True"
                        , "--"
                        , "--on", ""
                        ] 1
        -- cpu core temperature monitor
        , Run $ MultiCoreTemp       [ "--template" , "<box type=Bottom width=4 color=" ++ color3 ++ "> <fn=1>🌡</fn> <avg>°C </box>"
                             , "--Low"      , "70"        -- units: °C
                             , "--High"     , "80"        -- units: °C
                             , "--low"      , "green"
                             , "--normal"   , "orange"
                             , "--high"     , "red"
                             ] 50

        -- memory usage monitor
        , Run $ Memory       [ "--template" ,"<box type=Bottom width=4 color=" ++ color4 ++ "> <fn=1>\xf233</fn> <usedratio>% </box>"
                             , "--Low"      , "20"        -- units: %
                             , "--High"     , "90"        -- units: %
                             , "--low"      , "green"
                             , "--normal"   , "orange"
                             , "--high"     , "red"
                             ] 10

        -- battery monitor
        , Run $ Battery      [ "--template" , "<box type=Bottom width=4 color=" ++ color5 ++ "> <fn=1>\xf242</fn> <acstatus>% </box>"
                             , "--Low"      , "15"        -- units: %
                             , "--High"     , "75"        -- units: %
                             , "--low"      , "red"
                             , "--normal"   , "orange"
                             , "--high"     , "green"

                             , "--" -- battery specific options
                             -- discharging status
                                       , "-o" , "<left>"
                                       -- AC "on" status
                                       , "-O" , "<fn=1>⚡</fn> <fc=#dAA520><left></fc>"
                                       -- charged status
                                       , "-i" , "<fc=#006000>Charged</fc>"
                             ] 50

        -- time and date indicator
        --   (%F = y-m-d date, %a = day of week, %T = h:m:s time)
        , Run $ Date           "%a, %b %d %I:%M:%S %p" "date" 10
        -- , Run $ Date        ( "<box type=Bottom width=4 color=" ++ color8 ++ "> %a, %b %d %I:%M:%S %p </box>") "date" 10

        , Run XMonadLog
        -- weather monitor
        -- Run $ Weather "RJTT" [ "--template", "<skyCondition> | <fc=#4682B4><tempC></fc>°C | <fc=#4682B4><rh></fc>% | <fc=#4682B4><pressure></fc>hPa"
        --                      ] 36000

        -- network activity monitor (dynamic interface resolution)
        -- , Run $ DynNetwork     [ "--template" , "<fn=1>🖧</fn> <dev>: <tx>kB/s|<rx>kB/s"
        --                      , "--Low"      , "1000"       -- units: B/s
        --                      , "--High"     , "5000"       -- units: B/s
        --                      , "--low"      , "green"
        --                      , "--normal"   , "orange"
        --                      , "--high"     , "red"
        --                      ] 10

        -- keyboard layout indicator
        -- , Run $ Kbd            [ ("us(dvorak)" , "<fc=#00008B>DV</fc>")
        --                      , ("us"         , "<fc=#8B0000>US</fc>")
        --                      ]
        ]

   , template = " <icon=arch.xpm/> <fc=#666666>|</fc> %XMonadLog% } %date% { %default:Master%  %multicpu%  %multicoretemp%  %memory%  %battery% "
}

main :: IO ()
main = xmobar config
