module Main (main) where

import Xmobar
import Relude

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
      Run $ Date "%k:%M %a %m/%d/%y" "datetime" 10,
      Run $ Volume "default" "Master" [] 1, -- Using `Alsa` won't always pick up when the default sink changes.
      Run $ Alsa "default" "Capture" ["-t", "Mic: <volume>% <status>"]
    ],
  sepChar = "%",
  alignSep = "}{",
  template = " %StdinReader% }{ %cpu% %memory% Disk: %disku% | %default:Master% %alsa:default:Capture% | %datetime% "
}
