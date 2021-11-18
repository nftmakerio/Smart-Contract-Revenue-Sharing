{-# LANGUAGE RecordWildCards #-}

module Main where

import Cardano.Api hiding (TxId)
import Control.Monad
import Data.List (nub)
import Data.String
import Ledger hiding (singleton)
import Options.Applicative
import qualified PlutusTx.AssocMap as A
import System.Exit
import System.IO

import Canonical.RevenueSharing

data OptsA a = OptsA
  { sendTo :: a
  , output :: FilePath
  } deriving Show

type RawOpts = OptsA [(PubKeyHash, Integer)]
type Opts = OptsA Config

main :: IO ()
main = createSC =<< checkOpts =<< execParser opts

opts :: ParserInfo RawOpts
opts = info (optsParser <**> helper) . mconcat $
  [ fullDesc
  , progDesc "Create a smart contract for revenue sharing"
  ]

optsParser :: Parser RawOpts
optsParser = OptsA
  <$> (some . option parseToOpt . mconcat $
    [ long "to"
    , metavar "<addr>:<pct>"
    , help "Address to send to and the percent to send. Can appear multiple times. Percentages are times 10, e.g. 2.5% would be 25. Percentages must add up to 100% (i.e. 1000)."
    ])
  <*> (strOption . mconcat $
    [ long "output"
    , metavar "FILE"
    , help "Where to write the script."
    ])

parseToOpt :: ReadM (PubKeyHash, Integer)
parseToOpt = eitherReader $ \s ->
  let
    addr = takeWhile (/= ':') s
    pct = drop 1 . dropWhile (/= ':') $ s
  in
    case reads pct of
      [(r, "")] -> pure (fromString addr, r)
      _ -> Left $ "cannot parse value `" ++ pct ++ "'"

checkOpts :: RawOpts -> IO Opts
checkOpts OptsA{..} = do
  let
    addresses = fst <$> sendTo
    pcts = snd <$> sendTo

  when (nub addresses /= addresses) $ do
    hPutStrLn stderr "Address must be unique"
    exitFailure

  when (sum pcts /= 1000) $ do
    hPutStrLn stderr "Percentages must sum to 1000 (i.e. 100%)"
    exitFailure

  pure . OptsA (A.fromList sendTo) $ output

createSC :: Opts -> IO ()
createSC OptsA{..} = do
  print sendTo
  print output

  result <- writeFileTextEnvelope output Nothing . revenueSharing $ sendTo
  case result of
      Left err -> print $ displayError err
      Right () -> putStrLn $ "wrote validator to file " ++ output


