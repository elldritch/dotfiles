module Main (main) where

import Relude
import System.Directory (doesFileExist)
import Xmobar

hasBattery :: IO Bool
hasBattery = getAny . mconcat <$> mapM isBattery batteryIDs
 where
  batteryIDs = ["BAT", "BAT0", "BAT1", "BAT2"]
  isBattery batteryID = do
    let f = "/sys/class/power_supply/" <> batteryID <> "/type"
    exists <- doesFileExist f
    Any
      <$> if exists
        then (== "Battery") <$> readFile "/sys/class/power_supply/BAT0/type"
        else pure False

-- Check .../sys/class/net? `ip route`? `iw dev`?
-- Tower has a wifi card... maybe I should look for "wifi + no ethernet"?
hasWirelessInterface :: IO Bool
hasWirelessInterface = undefined


main :: IO ()
main = do
  batteryPowered <- hasBattery
  xmobar $
    defaultConfig
      { font = "xft:monospace"
      , bgColor = "black"
      , fgColor = "grey"
      , position = TopW L 100
      , commands =
          [ Run StdinReader
          , Run $ Cpu ["-L", "3", "-H", "70", "--normal", "green", "--high", "red"] 10
          , Run $ Memory ["-t", "Mem: <usedratio>%", "-L", "5", "-H", "70", "--normal", "green", "--high", "red"] 10
          , Run $
              DiskU
                [ ("/", "/: <used>/<size> (<usedp>%)")
                , ("/home", "/home: <used>/<size> (<usedp>%)")
                ]
                ["-L", "5", "-H", "70", "--normal", "green", "--high", "red"]
                10
          , Run $ Volume "default" "Master" [] 1 -- Using `Alsa` won't always pick up when the default sink changes.
          , Run $ Alsa "default" "Capture" ["-t", "Mic: <volume>% <status>"]
          , Run $
              Wireless
                ""
                [ "--Low"
                , "55"
                , "--High"
                , "80"
                , "--low"
                , "red"
                , "--normal"
                , "yellow"
                , "--high"
                , "green"
                ]
                10
          , Run $ Date "%k:%M %a %m/%d/%y" "datetime" 10
          ]
            ++ [ Run $
                Battery
                  [ "--template"
                  , "<left>%<acstatus>, <timeleft> left"
                  , "--Low"
                  , "40"
                  , "--High"
                  , "80"
                  , "--low"
                  , "red"
                  , "--normal"
                  , "yellow"
                  , "--high"
                  , "green"
                  , "--"
                  , "-O"
                  , " (Charging)"
                  , "-i"
                  , " (Charged)"
                  , "-o"
                  , ""
                  ]
                  60
               | batteryPowered
               ]
      , sepChar = "%"
      , alignSep = "}{"
      , template =
          " %StdinReader% }{ "
            ++ intercalate
              " | "
              ( [ "%cpu% %memory% Disk: %disku%"
                , "%default:Master% %alsa:default:Capture%"
                , "%wi%"
                ]
                  ++ ["%battery%" | batteryPowered]
                  ++ ["%datetime%"]
              )
            ++ " "
      }
