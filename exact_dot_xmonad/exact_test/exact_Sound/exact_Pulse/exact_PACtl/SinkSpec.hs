module Sound.Pulse.PACtl.SinkSpec (spec) where

import Relude

import Test.Hspec (Spec, it, pending)

spec :: Spec
spec = it "parses the output of `pactl list sinks`" $ do
  pending
