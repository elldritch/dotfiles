module Sound.Pulse.PACtl.CardSpec (spec) where

import Relude

import Test.Hspec (Spec, it, shouldBe)
import Text.RawString.QQ (r)

import Sound.Pulse.PACtl.Card (parseCard)

spec :: Spec
spec = it "parses the output of `pactl list cards`" $ do
  parseCard fixture `shouldBe` Right []

fixture :: String
fixture =
  [r|Card #0
	Name: alsa_card.pci-0000_01_00.1
	Driver: module-alsa-card.c
	Owner Module: 6
	Properties:
		alsa.card = "1"
		alsa.card_name = "HDA NVidia"
		alsa.long_card_name = "HDA NVidia at 0xdf080000 irq 17"
		alsa.driver_name = "snd_hda_intel"
		device.bus_path = "pci-0000:01:00.1"
		sysfs.path = "/devices/pci0000:00/0000:00:01.0/0000:01:00.1/sound/card1"
		device.bus = "pci"
		device.vendor.id = "10de"
		device.vendor.name = "NVIDIA Corporation"
		device.product.id = "10f0"
		device.product.name = "GP104 High Definition Audio Controller"
		device.string = "1"
		device.description = "GP104 High Definition Audio Controller"
		module-udev-detect.discovered = "1"
		device.icon_name = "audio-card-pci"
	Profiles:
		output:hdmi-stereo: Digital Stereo (HDMI) Output (sinks: 1, sources: 0, priority: 5900, available: no)
		output:hdmi-surround: Digital Surround 5.1 (HDMI) Output (sinks: 1, sources: 0, priority: 800, available: no)
		output:hdmi-surround71: Digital Surround 7.1 (HDMI) Output (sinks: 1, sources: 0, priority: 800, available: no)
		output:hdmi-stereo-extra1: Digital Stereo (HDMI 2) Output (sinks: 1, sources: 0, priority: 38468, available: yes)
		output:hdmi-stereo-extra2: Digital Stereo (HDMI 3) Output (sinks: 1, sources: 0, priority: 38468, available: yes)
		output:hdmi-stereo-extra3: Digital Stereo (HDMI 4) Output (sinks: 1, sources: 0, priority: 5700, available: no)
		output:hdmi-surround-extra3: Digital Surround 5.1 (HDMI 4) Output (sinks: 1, sources: 0, priority: 600, available: no)
		output:hdmi-surround71-extra3: Digital Surround 7.1 (HDMI 4) Output (sinks: 1, sources: 0, priority: 600, available: no)
		output:hdmi-stereo-extra4: Digital Stereo (HDMI 5) Output (sinks: 1, sources: 0, priority: 5700, available: no)
		output:hdmi-surround-extra4: Digital Surround 5.1 (HDMI 5) Output (sinks: 1, sources: 0, priority: 600, available: no)
		output:hdmi-surround71-extra4: Digital Surround 7.1 (HDMI 5) Output (sinks: 1, sources: 0, priority: 600, available: no)
		output:hdmi-stereo-extra5: Digital Stereo (HDMI 6) Output (sinks: 1, sources: 0, priority: 5700, available: no)
		output:hdmi-surround-extra5: Digital Surround 5.1 (HDMI 6) Output (sinks: 1, sources: 0, priority: 600, available: no)
		output:hdmi-surround71-extra5: Digital Surround 7.1 (HDMI 6) Output (sinks: 1, sources: 0, priority: 600, available: no)
		output:hdmi-stereo-extra6: Digital Stereo (HDMI 7) Output (sinks: 1, sources: 0, priority: 5700, available: no)
		output:hdmi-surround-extra6: Digital Surround 5.1 (HDMI 7) Output (sinks: 1, sources: 0, priority: 600, available: no)
		output:hdmi-surround71-extra6: Digital Surround 7.1 (HDMI 7) Output (sinks: 1, sources: 0, priority: 600, available: no)
		off: Off (sinks: 0, sources: 0, priority: 0, available: yes)
	Active Profile: output:hdmi-stereo-extra2
	Ports:
		hdmi-output-0: HDMI / DisplayPort (type: HDMI, priority: 5900, latency offset: 0 usec, not available)
			Properties:
				device.icon_name = "video-display"
			Part of profile(s): output:hdmi-stereo, output:hdmi-surround, output:hdmi-surround71
		hdmi-output-1: HDMI / DisplayPort 2 (type: HDMI, priority: 5800, latency offset: 0 usec, available)
			Properties:
				device.icon_name = "video-display"
				device.product.name = "DELL U2414H
 "
			Part of profile(s): output:hdmi-stereo-extra1
		hdmi-output-2: HDMI / DisplayPort 3 (type: HDMI, priority: 5700, latency offset: 0 usec, available)
			Properties:
				device.icon_name = "video-display"
				device.product.name = "DELL U2414H
 "
			Part of profile(s): output:hdmi-stereo-extra2
		hdmi-output-3: HDMI / DisplayPort 4 (type: HDMI, priority: 5600, latency offset: 0 usec, not available)
			Properties:
				device.icon_name = "video-display"
			Part of profile(s): output:hdmi-stereo-extra3, output:hdmi-surround-extra3, output:hdmi-surround71-extra3
		hdmi-output-4: HDMI / DisplayPort 5 (type: HDMI, priority: 5500, latency offset: 0 usec, not available)
			Properties:
				device.icon_name = "video-display"
			Part of profile(s): output:hdmi-stereo-extra4, output:hdmi-surround-extra4, output:hdmi-surround71-extra4
		hdmi-output-5: HDMI / DisplayPort 6 (type: HDMI, priority: 5400, latency offset: 0 usec, not available)
			Properties:
				device.icon_name = "video-display"
			Part of profile(s): output:hdmi-stereo-extra5, output:hdmi-surround-extra5, output:hdmi-surround71-extra5
		hdmi-output-6: HDMI / DisplayPort 7 (type: HDMI, priority: 5300, latency offset: 0 usec, not available)
			Properties:
				device.icon_name = "video-display"
			Part of profile(s): output:hdmi-stereo-extra6, output:hdmi-surround-extra6, output:hdmi-surround71-extra6

Card #2
	Name: alsa_card.pci-0000_00_1f.3
	Driver: module-alsa-card.c
	Owner Module: 8
	Properties:
		alsa.card = "0"
		alsa.card_name = "HDA Intel PCH"
		alsa.long_card_name = "HDA Intel PCH at 0xdf320000 irq 128"
		alsa.driver_name = "snd_hda_intel"
		device.bus_path = "pci-0000:00:1f.3"
		sysfs.path = "/devices/pci0000:00/0000:00:1f.3/sound/card0"
		device.bus = "pci"
		device.vendor.id = "8086"
		device.vendor.name = "Intel Corporation"
		device.product.id = "a170"
		device.product.name = "100 Series/C230 Series Chipset Family HD Audio Controller"
		device.form_factor = "internal"
		device.string = "0"
		device.description = "Built-in Audio"
		module-udev-detect.discovered = "1"
		device.icon_name = "audio-card-pci"
	Profiles:
		input:analog-stereo: Analog Stereo Input (sinks: 0, sources: 1, priority: 65, available: no)
		output:analog-stereo: Analog Stereo Output (sinks: 1, sources: 0, priority: 39268, available: yes)
		output:analog-stereo+input:analog-stereo: Analog Stereo Duplex (sinks: 1, sources: 1, priority: 6565, available: yes)
		output:analog-surround-21: Analog Surround 2.1 Output (sinks: 1, sources: 0, priority: 1300, available: no)
		output:analog-surround-21+input:analog-stereo: Analog Surround 2.1 Output + Analog Stereo Input (sinks: 1, sources: 1, priority: 1365, available: no)
		output:analog-surround-40: Analog Surround 4.0 Output (sinks: 1, sources: 0, priority: 1200, available: no)
		output:analog-surround-40+input:analog-stereo: Analog Surround 4.0 Output + Analog Stereo Input (sinks: 1, sources: 1, priority: 1265, available: no)
		output:analog-surround-41: Analog Surround 4.1 Output (sinks: 1, sources: 0, priority: 1300, available: no)
		output:analog-surround-41+input:analog-stereo: Analog Surround 4.1 Output + Analog Stereo Input (sinks: 1, sources: 1, priority: 1365, available: no)
		output:analog-surround-50: Analog Surround 5.0 Output (sinks: 1, sources: 0, priority: 1200, available: no)
		output:analog-surround-50+input:analog-stereo: Analog Surround 5.0 Output + Analog Stereo Input (sinks: 1, sources: 1, priority: 1265, available: no)
		output:analog-surround-51: Analog Surround 5.1 Output (sinks: 1, sources: 0, priority: 1300, available: no)
		output:analog-surround-51+input:analog-stereo: Analog Surround 5.1 Output + Analog Stereo Input (sinks: 1, sources: 1, priority: 1365, available: no)
		output:iec958-stereo: Digital Stereo (IEC958) Output (sinks: 1, sources: 0, priority: 38268, available: yes)
		output:iec958-stereo+input:analog-stereo: Digital Stereo (IEC958) Output + Analog Stereo Input (sinks: 1, sources: 1, priority: 5565, available: yes)
		output:iec958-ac3-surround-51: Digital Surround 5.1 (IEC958/AC3) Output (sinks: 1, sources: 0, priority: 300, available: yes)
		output:iec958-ac3-surround-51+input:analog-stereo: Digital Surround 5.1 (IEC958/AC3) Output + Analog Stereo Input (sinks: 1, sources: 1, priority: 365, available: no)
		off: Off (sinks: 0, sources: 0, priority: 0, available: yes)
	Active Profile: output:analog-stereo
	Ports:
		analog-input-front-mic: Front Microphone (type: Mic, priority: 8500, latency offset: 0 usec, not available)
			Properties:
				device.icon_name = "audio-input-microphone"
			Part of profile(s): input:analog-stereo, output:analog-stereo+input:analog-stereo, output:analog-surround-21+input:analog-stereo, output:analog-surround-40+input:analog-stereo, output:analog-surround-41+input:analog-stereo, output:analog-surround-50+input:analog-stereo, output:analog-surround-51+input:analog-stereo, output:iec958-stereo+input:analog-stereo, output:iec958-ac3-surround-51+input:analog-stereo
		analog-input-rear-mic: Rear Microphone (type: Mic, priority: 8200, latency offset: 0 usec, not available)
			Properties:
				device.icon_name = "audio-input-microphone"
			Part of profile(s): input:analog-stereo, output:analog-stereo+input:analog-stereo, output:analog-surround-21+input:analog-stereo, output:analog-surround-40+input:analog-stereo, output:analog-surround-41+input:analog-stereo, output:analog-surround-50+input:analog-stereo, output:analog-surround-51+input:analog-stereo, output:iec958-stereo+input:analog-stereo, output:iec958-ac3-surround-51+input:analog-stereo
		analog-input-linein: Line In (type: Line, priority: 8100, latency offset: 0 usec, not available)
			Part of profile(s): input:analog-stereo, output:analog-stereo+input:analog-stereo, output:analog-surround-21+input:analog-stereo, output:analog-surround-40+input:analog-stereo, output:analog-surround-41+input:analog-stereo, output:analog-surround-50+input:analog-stereo, output:analog-surround-51+input:analog-stereo, output:iec958-stereo+input:analog-stereo, output:iec958-ac3-surround-51+input:analog-stereo
		analog-output-lineout: Line Out (type: Line, priority: 9000, latency offset: 0 usec, not available)
			Part of profile(s): output:analog-stereo, output:analog-stereo+input:analog-stereo, output:analog-surround-21, output:analog-surround-21+input:analog-stereo, output:analog-surround-40, output:analog-surround-40+input:analog-stereo, output:analog-surround-41, output:analog-surround-41+input:analog-stereo, output:analog-surround-50, output:analog-surround-50+input:analog-stereo, output:analog-surround-51, output:analog-surround-51+input:analog-stereo
		analog-output-headphones: Headphones (type: Headphones, priority: 9900, latency offset: 0 usec, available)
			Properties:
				device.icon_name = "audio-headphones"
			Part of profile(s): output:analog-stereo, output:analog-stereo+input:analog-stereo
		iec958-stereo-output: Digital Output (S/PDIF) (type: SPDIF, priority: 0, latency offset: 0 usec, availability unknown)
			Part of profile(s): output:iec958-stereo, output:iec958-stereo+input:analog-stereo

Card #4
	Name: alsa_card.usb-Generic_Blue_Microphones_2126BAH02H08-00
	Driver: module-alsa-card.c
	Owner Module: 24
	Properties:
		alsa.card = "3"
		alsa.card_name = "Blue Microphones"
		alsa.long_card_name = "Generic Blue Microphones at usb-0000:00:14.0-8, high speed"
		alsa.driver_name = "snd_usb_audio"
		device.bus_path = "pci-0000:00:14.0-usb-0:8:1.0"
		sysfs.path = "/devices/pci0000:00/0000:00:14.0/usb1/1-8/1-8:1.0/sound/card3"
		udev.id = "usb-Generic_Blue_Microphones_2126BAH02H08-00"
		device.bus = "usb"
		device.vendor.id = "046d"
		device.vendor.name = "Logitech, Inc."
		device.product.id = "0ab7"
		device.product.name = "Blue Microphones"
		device.serial = "Generic_Blue_Microphones_2126BAH02H08"
		device.form_factor = "microphone"
		device.string = "3"
		device.description = "Blue Microphones"
		module-udev-detect.discovered = "1"
		device.icon_name = "audio-input-microphone-usb"
	Profiles:
		input:analog-stereo: Analog Stereo Input (sinks: 0, sources: 1, priority: 65, available: yes)
		input:iec958-stereo: Digital Stereo (IEC958) Input (sinks: 0, sources: 1, priority: 55, available: yes)
		output:analog-stereo: Analog Stereo Output (sinks: 1, sources: 0, priority: 6500, available: yes)
		output:analog-stereo+input:analog-stereo: Analog Stereo Duplex (sinks: 1, sources: 1, priority: 6565, available: yes)
		output:analog-stereo+input:iec958-stereo: Analog Stereo Output + Digital Stereo (IEC958) Input (sinks: 1, sources: 1, priority: 6555, available: yes)
		output:iec958-stereo: Digital Stereo (IEC958) Output (sinks: 1, sources: 0, priority: 5500, available: yes)
		output:iec958-stereo+input:analog-stereo: Digital Stereo (IEC958) Output + Analog Stereo Input (sinks: 1, sources: 1, priority: 5565, available: yes)
		output:iec958-stereo+input:iec958-stereo: Digital Stereo Duplex (IEC958) (sinks: 1, sources: 1, priority: 5555, available: yes)
		output:iec958-ac3-surround-51: Digital Surround 5.1 (IEC958/AC3) Output (sinks: 1, sources: 0, priority: 300, available: yes)
		output:iec958-ac3-surround-51+input:analog-stereo: Digital Surround 5.1 (IEC958/AC3) Output + Analog Stereo Input (sinks: 1, sources: 1, priority: 365, available: yes)
		output:iec958-ac3-surround-51+input:iec958-stereo: Digital Surround 5.1 (IEC958/AC3) Output + Digital Stereo (IEC958) Input (sinks: 1, sources: 1, priority: 355, available: yes)
		off: Off (sinks: 0, sources: 0, priority: 0, available: yes)
	Active Profile: output:analog-stereo+input:analog-stereo
	Ports:
		analog-input-mic: Microphone (type: Mic, priority: 8700, latency offset: 0 usec, availability unknown)
			Properties:
				device.icon_name = "audio-input-microphone"
			Part of profile(s): input:analog-stereo, output:analog-stereo+input:analog-stereo, output:iec958-stereo+input:analog-stereo, output:iec958-ac3-surround-51+input:analog-stereo
		iec958-stereo-input: Digital Input (S/PDIF) (type: SPDIF, priority: 0, latency offset: 0 usec, availability unknown)
			Part of profile(s): input:iec958-stereo, output:analog-stereo+input:iec958-stereo, output:iec958-stereo+input:iec958-stereo, output:iec958-ac3-surround-51+input:iec958-stereo
		analog-output-speaker: Speakers (type: Speaker, priority: 10000, latency offset: 0 usec, availability unknown)
			Properties:
				device.icon_name = "audio-speakers"
			Part of profile(s): output:analog-stereo, output:analog-stereo+input:analog-stereo, output:analog-stereo+input:iec958-stereo
		iec958-stereo-output: Digital Output (S/PDIF) (type: SPDIF, priority: 0, latency offset: 0 usec, availability unknown)
			Part of profile(s): output:iec958-stereo, output:iec958-stereo+input:analog-stereo, output:iec958-stereo+input:iec958-stereo

Card #5
	Name: alsa_card.usb-046d_HD_Pro_Webcam_C920_0F9C44AF-02
	Driver: module-alsa-card.c
	Owner Module: 25
	Properties:
		alsa.card = "2"
		alsa.card_name = "HD Pro Webcam C920"
		alsa.long_card_name = "HD Pro Webcam C920 at usb-0000:00:14.0-10, high speed"
		alsa.driver_name = "snd_usb_audio"
		device.bus_path = "pci-0000:00:14.0-usb-0:10:1.2"
		sysfs.path = "/devices/pci0000:00/0000:00:14.0/usb1/1-10/1-10:1.2/sound/card2"
		udev.id = "usb-046d_HD_Pro_Webcam_C920_0F9C44AF-02"
		device.bus = "usb"
		device.vendor.id = "046d"
		device.vendor.name = "Logitech, Inc."
		device.product.id = "082d"
		device.product.name = "HD Pro Webcam C920"
		device.serial = "046d_HD_Pro_Webcam_C920_0F9C44AF"
		device.form_factor = "webcam"
		device.string = "2"
		device.description = "HD Pro Webcam C920"
		module-udev-detect.discovered = "1"
		device.icon_name = "camera-web-usb"
	Profiles:
		input:analog-stereo: Analog Stereo Input (sinks: 0, sources: 1, priority: 65, available: yes)
		input:iec958-stereo: Digital Stereo (IEC958) Input (sinks: 0, sources: 1, priority: 55, available: yes)
		off: Off (sinks: 0, sources: 0, priority: 0, available: yes)
	Active Profile: input:analog-stereo
	Ports:
		analog-input-mic: Microphone (type: Mic, priority: 8700, latency offset: 0 usec, availability unknown)
			Properties:
				device.icon_name = "audio-input-microphone"
			Part of profile(s): input:analog-stereo
		iec958-stereo-input: Digital Input (S/PDIF) (type: SPDIF, priority: 0, latency offset: 0 usec, availability unknown)
			Part of profile(s): input:iec958-stereo
|]
