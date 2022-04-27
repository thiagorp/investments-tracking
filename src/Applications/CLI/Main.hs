{-# LANGUAGE TypeApplications #-}

module Applications.CLI.Main where

import Applications.CLI.DataSources.Binance (readLedger)
import Applications.CLI.Import
import Investments.Domain (Asset (Asset), assetsBalance)
import RIO.Map qualified as Map
import System.IO (print)

printBalance :: (Asset, Rational) -> App ()
printBalance (Asset assetName, rationalBalance) =
  logInfo $ display assetName <> ": " <> display balance
 where
  balance =
    if rationalBalance >= 0.00000001
      then fromRational @Double rationalBalance
      else 0

run :: App ()
run = do
  ledger <- readLedger
  let balances = assetsBalance ledger
  traverse_ printBalance (Map.toList (Map.filter (>= 0.00000001) balances))

main :: IO ()
main = do
  eSettings <- loadSettings
  case eSettings of
    Left err -> print err
    Right settings -> runApp settings run