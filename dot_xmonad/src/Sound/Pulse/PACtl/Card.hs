module Sound.Pulse.PACtl.Card (parseCard) where

import Relude

import Control.Monad.Error (throwError)
import Control.Monad.State (withStateT)
import Text.Megaparsec (ParsecT, anySingle, anySingleBut, between, choice, chunk, eof, errorBundlePretty, noneOf, runParserT, sepBy1, single, someTill, try, withRecovery)
import Text.Megaparsec.Char (newline, tab)
import Text.Megaparsec.Debug (dbg)

type Parser = ParsecT Void String (State Int)

data PulseAudioCard = PulseAudioCard
  { pulseAudioCardName :: String
  , alsaCardID :: String
  , profiles :: [PulseAudioCardProfile]
  }
  deriving (Eq, Show)

data PulseAudioCardProfile = PulseAudioCardProfile
  { profileID :: String
  , name :: String
  , available :: Bool
  }
  deriving (Eq, Show)

data Field
  = Single (String, String)
  | Section String [Field]
  | Properties String [(String, String)]
  deriving (Show)

-- What if I just wrote some small C programs to shell out to instead? Instead of
-- trying to parse or write bindings.

parseCard :: String -> Either String [PulseAudioCard]
parseCard = first errorBundlePretty . evaluatingState 0 . runParserT (sepBy1 cardP newline <* eof) ""
  where
    cardP :: Parser PulseAudioCard
    cardP = dbg "cardP" $ do
      -- This matches the card fine right now, and then hits the `undefined` and
      -- dies.
      --
      -- The main problem is that the ports aren't properly parsed, because the
      -- section title with a `: ` inside of it gets parsed as a KV.
      --
      -- The problem is that if a section title parses as a valid KV, I can't
      -- tell Megaparsec "hey, backtrack this successful KV parse to try parsing
      -- as a section header".
      --
      -- I might need a new parser alternative (`section'`) that handles
      -- KV-parseable section titles and is tried before KV.
      --
      -- Or maybe it's enough to just try sections first, since sections always
      -- have at least 1 field?
      _ <- fieldP
      undefined

    nested :: Parser a -> Parser a
    nested p = do
      indent
      ret <- withRecovery (\_ -> unindent >> mzero) p
      unindent
      pure ret

    indent :: Parser ()
    indent = do
      indentation <- get
      put $ indentation + 1

    unindent :: Parser ()
    unindent = do
      indentation <- get
      put $ indentation - 1

    indented :: Parser a -> Parser a
    indented p = do
      indentation <- traceShowWith (\i -> "indentation: " <> show i) <$> get
      replicateM_ indentation tab
      p

    lineChar :: Parser Char
    lineChar = noneOf ['\n', '\t']

    fieldP :: Parser Field
    fieldP =
      dbg "fieldP" $
        choice
          [ uncurry Section <$> section
          , uncurry Properties <$> props
          , Single <$> kv
          ]

    -- Matches `key: value`.
    kv :: Parser (String, String)
    kv = dbg "kv" $
      try $
        indented $ do
          key <- dbg "kv.key" $ someTill lineChar $ chunk ": "
          value <- dbg "kv.value" $ someTill lineChar newline
          pure (key, value)

    -- Matches `key = "value"`.
    -- TODO: This does not support quote escaping. Should probably use lexing to handle that.
    prop :: Parser (String, String)
    prop = dbg "prop" $
      try $
        indented $ do
          key <- dbg "prop.key" $ someTill lineChar $ chunk " = "
          value <- dbg "prop.value" $ between (single '"') (single '"') $ many $ anySingleBut '"'
          _ <- newline
          pure (key, value)

    props :: Parser (String, [(String, String)])
    props = dbg "props" $
      try $
        indented $ do
          title <- dbg "props.title" $ someTill lineChar newline
          values <- dbg "props.values" $ nested $ some prop
          pure (title, values)

    section :: Parser (String, [Field])
    section = dbg "section" $
      try $
        indented $ do
          title <- dbg "section.title" $ someTill lineChar newline
          fields <- dbg "section.fields" $ nested $ some fieldP
          pure (title, fields)

x =
  Section
    "Card #0"
    [ Single ("Name", "alsa_card.pci-0000_01_00.1")
    , Single ("Driver", "module-alsa-card.c")
    , Single ("Owner Module", "6")
    , Properties
        "Properties:"
        [ ("alsa.card", "1")
        , ("alsa.card_name", "HDA NVidia")
        , ("alsa.long_card_name", "HDA NVidia at 0xdf080000 irq 17")
        , ("alsa.driver_name", "snd_hda_intel")
        , ("device.bus_path", "pci-0000:01:00.1")
        , ("sysfs.path", "/devices/pci0000:00/0000:00:01.0/0000:01:00.1/sound/card1")
        , ("device.bus", "pci")
        , ("device.vendor.id", "10de")
        , ("device.vendor.name", "NVIDIA Corporation")
        , ("device.product.id", "10f0")
        , ("device.product.name", "GP104 High Definition Audio Controller")
        , ("device.string", "1")
        , ("device.description", "GP104 High Definition Audio Controller")
        , ("module-udev-detect.discovered", "1")
        , ("device.icon_name", "audio-card-pci")
        ]
    , Section
        "Profiles:"
        [ Single ("output:hdmi-stereo", "Digital Stereo (HDMI) Output (sinks: 1, sources: 0, priority: 5900, available: no)")
        , Single ("output:hdmi-surround", "Digital Surround 5.1 (HDMI) Output (sinks: 1, sources: 0, priority: 800, available: no)")
        , Single ("output:hdmi-surround71", "Digital Surround 7.1 (HDMI) Output (sinks: 1, sources: 0, priority: 800, available: no)")
        , Single ("output:hdmi-stereo-extra1", "Digital Stereo (HDMI 2) Output (sinks: 1, sources: 0, priority: 38468, available: yes)")
        , Single ("output:hdmi-stereo-extra2", "Digital Stereo (HDMI 3) Output (sinks: 1, sources: 0, priority: 38468, available: yes)")
        , Single ("output:hdmi-stereo-extra3", "Digital Stereo (HDMI 4) Output (sinks: 1, sources: 0, priority: 5700, available: no)")
        , Single ("output:hdmi-surround-extra3", "Digital Surround 5.1 (HDMI 4) Output (sinks: 1, sources: 0, priority: 600, available: no)")
        , Single ("output:hdmi-surround71-extra3", "Digital Surround 7.1 (HDMI 4) Output (sinks: 1, sources: 0, priority: 600, available: no)")
        , Single ("output:hdmi-stereo-extra4", "Digital Stereo (HDMI 5) Output (sinks: 1, sources: 0, priority: 5700, available: no)")
        , Single ("output:hdmi-surround-extra4", "Digital Surround 5.1 (HDMI 5) Output (sinks: 1, sources: 0, priority: 600, available: no)")
        , Single ("output:hdmi-surround71-extra4", "Digital Surround 7.1 (HDMI 5) Output (sinks: 1, sources: 0, priority: 600, available: no)")
        , Single ("output:hdmi-stereo-extra5", "Digital Stereo (HDMI 6) Output (sinks: 1, sources: 0, priority: 5700, available: no)")
        , Single ("output:hdmi-surround-extra5", "Digital Surround 5.1 (HDMI 6) Output (sinks: 1, sources: 0, priority: 600, available: no)")
        , Single ("output:hdmi-surround71-extra5", "Digital Surround 7.1 (HDMI 6) Output (sinks: 1, sources: 0, priority: 600, available: no)")
        , Single ("output:hdmi-stereo-extra6", "Digital Stereo (HDMI 7) Output (sinks: 1, sources: 0, priority: 5700, available: no)")
        , Single ("output:hdmi-surround-extra6", "Digital Surround 5.1 (HDMI 7) Output (sinks: 1, sources: 0, priority: 600, available: no)")
        , Single ("output:hdmi-surround71-extra6", "Digital Surround 7.1 (HDMI 7) Output (sinks: 1, sources: 0, priority: 600, available: no)")
        , Single ("off", "Off (sinks: 0, sources: 0, priority: 0, available: yes)")
        ]
    , Single ("Active Profile", "output:hdmi-stereo-extra2")
    , Section
        "Ports:"
        [ Section
            "hdmi-output-0: HDMI / DisplayPort (type: HDMI, priority: 5900, latency offset: 0 usec, not available)"
            [ Properties "Properties:" [("device.icon_name", "video-display")]
            , Single ("Part of profile(s)", "output:hdmi-stereo, output:hdmi-surround, output:hdmi-surround71")
            ]
        , Section
            "hdmi-output-1: HDMI / DisplayPort 2 (type: HDMI, priority: 5800, latency offset: 0 usec, available)"
            [ Properties "Properties:" [("device.icon_name", "video-display"), ("device.product.name", "DELL U2414H\n ")]
            , Single ("Part of profile(s)", "output:hdmi-stereo-extra1")
            ]
        , Section
            "hdmi-output-2: HDMI / DisplayPort 3 (type: HDMI, priority: 5700, latency offset: 0 usec, available)"
            [ Properties "Properties:" [("device.icon_name", "video-display"), ("device.product.name", "DELL U2414H\n ")]
            , Single ("Part of profile(s)", "output:hdmi-stereo-extra2")
            ]
        , Section
            "hdmi-output-3: HDMI / DisplayPort 4 (type: HDMI, priority: 5600, latency offset: 0 usec, not available)"
            [ Properties "Properties:" [("device.icon_name", "video-display")]
            , Single ("Part of profile(s)", "output:hdmi-stereo-extra3, output:hdmi-surround-extra3, output:hdmi-surround71-extra3")
            ]
        , Section
            "hdmi-output-4: HDMI / DisplayPort 5 (type: HDMI, priority: 5500, latency offset: 0 usec, not available)"
            [ Properties "Properties:" [("device.icon_name", "video-display")]
            , Single ("Part of profile(s)", "output:hdmi-stereo-extra4, output:hdmi-surround-extra4, output:hdmi-surround71-extra4")
            ]
        , Section
            "hdmi-output-5: HDMI / DisplayPort 6 (type: HDMI, priority: 5400, latency offset: 0 usec, not available)"
            [ Properties "Properties:" [("device.icon_name", "video-display")]
            , Single ("Part of profile(s)", "output:hdmi-stereo-extra5, output:hdmi-surround-extra5, output:hdmi-surround71-extra5")
            ]
        , Section
            "hdmi-output-6: HDMI / DisplayPort 7 (type: HDMI, priority: 5300, latency offset: 0 usec, not available)"
            [ Properties "Properties:" [("device.icon_name", "video-display")]
            , Single ("Part of profile(s)", "output:hdmi-stereo-extra6, output:hdmi-surround-extra6, output:hdmi-surround71-extra6")
            ]
        ]
    ]
