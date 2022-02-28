module Sound.Pulse.DBus.Sink (
  Sink (..),
  SinkID,
  getSinks,
  setDefaultSink,
) where

import Relude

import DBus (InterfaceName, MemberName, ObjectPath, Variant, toVariant)

import Sound.Pulse.DBus (PulseAudioT)
import Sound.Pulse.DBus.Internal (call, fromPropList, fromVariantMap, getAllProperties, getProperty, setProperty)

data Sink = Sink
  { sinkID :: SinkID
  , name :: Text
  , description :: Maybe Text
  }
  deriving (Show)

type SinkID = ObjectPath

coreObject :: ObjectPath
coreObject = "/org/pulseaudio/core1"

coreInterface :: InterfaceName
coreInterface = "org.PulseAudio.Core1"

sinksProperty :: MemberName
sinksProperty = "Sinks"

deviceInterface :: InterfaceName
deviceInterface = "org.PulseAudio.Core1.Device"

getSinks :: (MonadIO m) => PulseAudioT m [Sink]
getSinks = do
  sinkPaths :: [ObjectPath] <- getProperty coreObject coreInterface sinksProperty
  forM sinkPaths $ \sinkID -> do
    sinkMap :: Map Text Variant <- getAllProperties sinkID deviceInterface
    name :: Text <- fromVariantMap "Name" sinkMap
    propsList :: Map Text ByteString <- fromVariantMap "PropertyList" sinkMap
    description <- fromPropList "device.description" propsList
    pure Sink{sinkID, name, description}

defaultSinkProperty :: MemberName
defaultSinkProperty = "FallbackSink"

setDefaultSink :: (MonadIO m) => SinkID -> PulseAudioT m ()
setDefaultSink sinkID = do
  movePlaybackStreamsTo sinkID
  setProperty coreObject coreInterface defaultSinkProperty sinkID

playbackStreamsProperty :: MemberName
playbackStreamsProperty = "PlaybackStreams"

streamInterface :: InterfaceName
streamInterface = "org.PulseAudio.Core1.Stream"

moveMethod :: MemberName
moveMethod = "Move"

movePlaybackStreamsTo :: (MonadIO m) => SinkID -> PulseAudioT m ()
movePlaybackStreamsTo sinkID = do
  pbStreamPaths :: [ObjectPath] <- getProperty coreObject coreInterface playbackStreamsProperty
  forM_ pbStreamPaths $ \pbStreamPath -> do
    call pbStreamPath streamInterface moveMethod [toVariant sinkID]
