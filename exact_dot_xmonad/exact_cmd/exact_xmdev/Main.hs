-- | Useful for testing new utilities as a script instead when developing for
-- XMonad.
module Main (main) where

import Relude

import DBus (ObjectPath)
import Data.Map.Lazy qualified as Map
import Relude.Unsafe (fromJust)
import Sound.Pulse.DBus (runPulseAudioT)
import Sound.Pulse.DBus.Server (getPulseAudioServerAddress)
import Sound.Pulse.DBus.Sink (Sink (..), getSinks, setDefaultSink)
import XMonad.Util.Dmenu (dmenu)

main :: IO ()
main = testPickSink

testPickSink :: IO ()
testPickSink = do
  result <- runExceptT $ do
    addr <- ExceptT getPulseAudioServerAddress
    ExceptT $
      flip runPulseAudioT addr $ do
        sinks <- getSinks
        let nameToID = mapNameToID sinks
        selected <- toText <$> dmenu (toString . sinkName <$> sinks)
        print selected
        let sinkID = fromJust $ Map.lookup selected nameToID
        print sinkID
        setDefaultSink sinkID
  case result of
    Right r -> do
      putStrLn $ "Result:" <> show r
      putStrLn "OK"
    Left err -> putStrLn $ "ERROR: " <> err
  where
    sinkName :: Sink -> Text
    sinkName Sink{name, description} = fromMaybe name description

    mapNameToID :: [Sink] -> Map Text ObjectPath
    mapNameToID sinks = Map.fromList $ (\s@Sink{sinkID} -> (sinkName s, sinkID)) <$> sinks
