{-# LANGUAGE TypeApplications #-}

module Applications.CLI.Main where

import Applications.CLI.DataSources.Binance qualified as Binance
import Applications.CLI.DataSources.Bittrex qualified as Bittrex
import Applications.CLI.Import
import Investments.Main (Asset (Asset), assetsBalance, mergeLedgers, startLedger)
import RIO.Map qualified as Map
import System.IO (print)

printBalance :: (Asset, Rational) -> App ()
printBalance (Asset assetName, rationalBalance) =
  logInfo $ display assetName <> ": " <> display (tshow balance)
 where
  balance =
    if rationalBalance >= 0.00000001
      then fromRational @Double rationalBalance
      else 0

run :: App ()
run = do
  ledgers <-
    sequence
      [ Binance.readLedger
      , Bittrex.readLedger
      ]

  let balances = assetsBalance $ foldl' mergeLedgers startLedger ledgers
  traverse_ printBalance (Map.toList (Map.filter (>= 0.00000001) balances))

main :: IO ()
main = do
  eSettings <- loadSettings
  case eSettings of
    Left err -> print err
    Right settings -> runApp settings run