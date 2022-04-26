{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Applications.CLI.DataSources.Bittrex (readLedger) where

import Applications.CLI.DataSources.Common
import Applications.CLI.Import
import Data.Csv
import Investments.Main
import RIO.ByteString.Lazy qualified as LBS
import RIO.Text qualified as T

data Bittrex

instance FromNamedRecord (TradeRow Bittrex) where
  parseNamedRecord row = do
    assets <- row .: "Exchange"
    orderType <- row .: "OrderType"
    pricePerUnit <- toRational @Double <$> row .: "PricePerUnit"
    quantity <- toRational @Double <$> row .: "Quantity"
    tradeFee <- toRational @Double <$> row .: "Commission"

    let asset1 = Asset $ T.take 3 assets
        asset2 = Asset $ T.drop 4 assets
        (tradeBoughtAsset, tradeSoldAsset, tradeFeeAsset, tradePrice, tradeQuantity) =
          if orderType == ("LIMIT_BUY" :: Text)
            then (asset2, asset1, asset1, pricePerUnit, quantity)
            else (asset1, asset2, asset1, 1 / pricePerUnit, quantity * pricePerUnit)

    pure TradeRow{..}

instance FromNamedRecord (DepositRow Bittrex) where
  parseNamedRecord row = do
    depositAsset <- Asset <$> row .: "Asset"
    depositQuantity <- toRational @Double <$> row .: "Quantity"
    pure DepositRow{..}

instance FromNamedRecord (WithdrawalRow Bittrex) where
  parseNamedRecord row = do
    withdrawalAsset <- Asset <$> row .: "Asset"
    withdrawalQuantity <- toRational @Double <$> row .: "Quantity"
    withdrawalFee <- toRational @Double <$> row .: "Fee"
    pure WithdrawalRow{..}

type TradeRows = Vector (TradeRow Bittrex)
type DepositRows = Vector (DepositRow Bittrex)
type WithdrawalRows = Vector (WithdrawalRow Bittrex)

loadTrades :: App (Either String TradeRows)
loadTrades = do
  dir <- dataDir <$> appSettings
  fileContents <- readFileBinary (dir <> "/bittrex/trades.csv")
  pure $ snd <$> decodeByName (LBS.fromStrict fileContents)

loadDeposits :: App (Either String DepositRows)
loadDeposits = do
  dir <- dataDir <$> appSettings
  fileContents <- readFileBinary (dir <> "/bittrex/deposits.csv")
  pure $ snd <$> decodeByName (LBS.fromStrict fileContents)

loadWithdrawals :: App (Either String WithdrawalRows)
loadWithdrawals = do
  dir <- dataDir <$> appSettings
  fileContents <- readFileBinary (dir <> "/bittrex/withdrawals.csv")
  pure $ snd <$> decodeByName (LBS.fromStrict fileContents)

loadData :: App (Either String (TradeRows, DepositRows, WithdrawalRows))
loadData = do
  trades <- loadTrades
  deposits <- loadDeposits
  withdrawals <- loadWithdrawals
  pure $ (,,) <$> trades <*> deposits <*> withdrawals

readLedger :: App Ledger
readLedger = do
  eData <- loadData
  case eData of
    Left err -> logError (displayShow err) >> pure startLedger
    Right (trades, deposits, withdrawals) ->
      pure $
        startLedger
          & addTrades trades
          & addDeposits deposits
          & addWithdrawals withdrawals