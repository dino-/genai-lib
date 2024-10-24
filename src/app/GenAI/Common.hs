{-# LANGUAGE OverloadedRecordDot #-}

module GenAI.Common
  where

import Data.Aeson (Value (Object))
import Data.Aeson.Types (emptyObject)
import Data.Text.Lazy qualified as TL
import GenAILib.Common (Host, LLMOptions (..), Model, OllamaRequest (..),
  Prompt (..), RawOutput, Stream, System)
import GenAILib.System.Log (Priority (DEBUG))


data Options = Options
  { host :: Host
  , model :: Model
  , system :: Maybe System
  , llmOptions :: LLMOptions
  , stream :: Stream
  , rawOutput :: RawOutput
  , verbose :: Verbose
  }


newtype Verbose = Verbose Bool
  deriving Show


verbosityToPriority :: Verbose -> Maybe Priority
verbosityToPriority (Verbose True) = Just DEBUG
verbosityToPriority (Verbose False) = Nothing


mkLLMRequest :: Options -> TL.Text -> OllamaRequest
mkLLMRequest opts promptText = OllamaRequest opts.model opts.system
  (Prompt promptText) opts.stream (wrapMaybe opts.llmOptions.v)
  where
    wrapMaybe o@(Object _) = if o == emptyObject then Nothing else Just o
    wrapMaybe _ = Nothing
