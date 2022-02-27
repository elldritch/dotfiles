module Sound.Pulse.PACtl.SourceSpec (spec) where

import Relude

import Test.Hspec (Spec, it, pending)

spec :: Spec
spec = it "parses the output of `pactl list sources`" $ do
  pending
