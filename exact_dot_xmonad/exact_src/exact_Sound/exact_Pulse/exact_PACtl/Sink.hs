module Sound.Pulse.PACtl.Sink () where

import Relude

data PulseAudioSink = PulseAudioSink
  { pulseAudioSinkName :: String
  , pulseAudioSinkDescription :: String
  }
