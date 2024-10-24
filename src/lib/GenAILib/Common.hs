{-# LANGUAGE DuplicateRecordFields #-}

module GenAILib.Common
  where

import Control.Arrow ( (&&&) )
import Control.Monad ( when )
import Data.Aeson ( Key, ToJSON, Value (Number, String),
  defaultOptions, genericToEncoding, genericToJSON, object, omitNothingFields,
  toEncoding, toJSON )
import Data.Aeson qualified as Aeson
import Data.Aeson.Key ( fromString )
import Data.Aeson.Types (Pair)
import Data.Maybe ( mapMaybe )
import Data.String.Conv ( toS )
import Data.Text.Lazy qualified as TL
import Formatting ( (%), formatToString, int, text )
import GHC.Generics ( Generic )
import Text.Read ( readMaybe )


newtype Model = Model TL.Text
  deriving Generic

instance ToJSON Model

defaultModel :: String
defaultModel = "llama3.1:8b"


newtype Prompt = Prompt TL.Text
  deriving Generic

instance ToJSON Prompt


newtype Stream = Stream Bool
  deriving Generic

instance ToJSON Stream


{- ^ This data structure is used to construct the POST body for an Ollama REST call
-}
data OllamaRequest = OllamaRequest
  { model :: Model
  , system :: Maybe System
  , prompt :: Prompt
  , stream :: Stream
  , options :: Maybe Value
  }
  deriving Generic

customOptions :: Aeson.Options
customOptions = defaultOptions { omitNothingFields = True }

instance ToJSON OllamaRequest where
  toJSON     = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions


splitAtColon :: String -> Maybe (String, String)
splitAtColon combinedStr = do
  let (leftSide, rightSide) = takeWhile (/= ':') &&& dropWhile (/= ':') $ combinedStr
  when (null rightSide) Nothing
  pure (leftSide, tail rightSide)


data Host = Host TL.Text Int

instance Show Host where
  show (Host hostName port) = formatToString (text % ":" % int) hostName port


defaultHost :: Host
defaultHost = Host "localhost" 11434


hostFromString :: String -> Maybe Host
hostFromString combinedStr = do
  (hostName, portStr) <- splitAtColon combinedStr
  Host (toS hostName) <$> readMaybe portStr


newtype System = System TL.Text
  deriving Generic

instance ToJSON System


newtype RawOutput = RawOutput Bool


newtype LLMOptions = LLMOptions { v :: Value }
  deriving Generic

instance ToJSON LLMOptions


-- For debugging the LLM options parsing, unused most of the time
debugOptsJSON :: LLMOptions -> Value
debugOptsJSON = toJSON

convertOptions :: [String] -> LLMOptions
convertOptions = LLMOptions . object . pairs'

pairs' :: [String] -> [Pair]
pairs' = map convertTypes . mapMaybe splitAtColon

convertTypes :: (String, String) -> (Key, Value)
-- Turns out the only non-number option we're interested in for Ollama is "stop"
convertTypes (keystr@"stop", valstr) = (fromString keystr, String . toS $ valstr)
convertTypes (keystr, valstr) = (fromString keystr, Number . read $ valstr)
