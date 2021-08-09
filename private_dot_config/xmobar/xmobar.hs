module Main (main) where

import Xmobar
import Relude

hasBattery :: IO Bool
hasBattery = undefined
-- Check /sys/class/power_supply/BAT0/type == Battery

hasWirelessInterface :: IO Bool
hasWirelessInterface = undefined
-- Check .../sys/class/net? `ip route`?
-- Tower has a wifi card... maybe I should look for "wifi + no ethernet"?

main :: IO ()
main = xmobar $ defaultConfig {
  font = "xft:monospace",
  bgColor = "black",
  fgColor = "grey",
  position = TopW L 100,
  commands = [
      Run StdinReader,
      Run $ Cpu ["-L", "3", "-H", "70", "--normal", "green", "--high", "red"] 10,
      Run $ Memory ["-t", "Mem: <usedratio>%", "-L", "5", "-H", "70", "--normal", "green", "--high", "red"] 10,
      Run $ DiskU [
          ("/", "/: <used>/<size> (<usedp>%)"),
          ("/home", "/home: <used>/<size> (<usedp>%)")
        ]
        ["-L", "5", "-H", "70", "--normal", "green", "--high", "red"] 10,
      Run $ Volume "default" "Master" [] 1, -- Using `Alsa` won't always pick up when the default sink changes.
      Run $ Alsa "default" "Capture" ["-t", "Mic: <volume>% <status>"],
      Run $ Wireless "" [
        "--Low", "55", "--High", "80",
        "--low", "red", "--normal", "yellow", "--high", "green"
      ] 10,
      -- TODO: Automatically add or remove this by detecting whether this device has a battery?
      Run $ Battery [
        "--template", "<left>%<acstatus>, <timeleft> left",
        "--Low", "40", "--High", "80",
        "--low", "red", "--normal", "yellow", "--high", "green",
        "--",
        "-O", " (Charging)", "-i", " (Charged)", "-o", ""
      ] 60,
      Run $ Date "%k:%M %a %m/%d/%y" "datetime" 10
    ],
  sepChar = "%",
  alignSep = "}{",
  template = " %StdinReader% }{ %cpu% %memory% Disk: %disku% | %default:Master% %alsa:default:Capture% | %wi% | %battery% | %datetime% "
}
