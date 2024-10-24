{-# LANGUAGE OverloadedRecordDot #-}

import Data.Text.Lazy.IO qualified as TL
import GenAILib.HTTP (display, doCompletion)
import GenAILib.System.Log (infoM, initLogging, lname)
import System.IO (stdin)

import GenAI.Common (Options (host, rawOutput, verbose), mkLLMRequest,
  verbosityToPriority)
import GenAI.Opts (parseOpts)


main :: IO ()
main = do
  opts <- parseOpts
  initLogging . verbosityToPriority . verbose $ opts
  infoM lname "Logging configured"
  infoM lname "Args parsed"

  promptText <- TL.hGetContents stdin
  let or' = mkLLMRequest opts promptText
  doCompletion opts.host or' >>= display opts.rawOutput
