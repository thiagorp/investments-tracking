{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Applications.CLI.DataSources.Bittrex (readLedger) where

import Applications.CLI.Import
import Data.Csv
import Investments.Main
import RIO.ByteString.Lazy qualified as LBS
import RIO.Text qualified as T

data TradeRow = TradeRow
  { tradeBoughtAsset :: Asset
  , tradeSoldAsset :: Asset
  , tradeQuantity :: Rational
  , tradeFee :: Rational
  , tradeFeeAsset :: Asset
  , tradePrice :: Rational
  }
  deriving (Show)

instance FromNamedRecord TradeRow where
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

data DepositRow = DepositRow
  { depositAsset :: Asset
  , depositQuantity :: Rational
  }

instance FromNamedRecord DepositRow where
  parseNamedRecord row = do
    depositAsset <- Asset <$> row .: "Asset"
    depositQuantity <- toRational @Double <$> row .: "Quantity"
    pure DepositRow{..}

data WithdrawalRow = WithdrawalRow
  { withdrawalAsset :: Asset
  , withdrawalQuantity :: Rational
  , withdrawalFee :: Rational
  }

instance FromNamedRecord WithdrawalRow where
  parseNamedRecord row = do
    withdrawalAsset <- Asset <$> row .: "Asset"
    withdrawalQuantity <- toRational @Double <$> row .: "Quantity"
    withdrawalFee <- toRational @Double <$> row .: "Fee"
    pure WithdrawalRow{..}

loadTradesData :: App (Either String (Vector TradeRow))
loadTradesData = do
  dir <- dataDir <$> appSettings
  fileContents <- readFileBinary (dir <> "/bittrex/trades.csv")
  pure $ snd <$> decodeByName (LBS.fromStrict fileContents)

loadDepositsData :: App (Either String (Vector DepositRow))
loadDepositsData = do
  dir <- dataDir <$> appSettings
  fileContents <- readFileBinary (dir <> "/bittrex/deposits.csv")
  pure $ snd <$> decodeByName (LBS.fromStrict fileContents)

loadWithdrawalsData :: App (Either String (Vector WithdrawalRow))
loadWithdrawalsData = do
  dir <- dataDir <$> appSettings
  fileContents <- readFileBinary (dir <> "/bittrex/withdrawals.csv")
  pure $ snd <$> decodeByName (LBS.fromStrict fileContents)

loadData :: App (Either String (Vector TradeRow, Vector DepositRow, Vector WithdrawalRow))
loadData = do
  tradesData <- loadTradesData
  depositsData <- loadDepositsData
  withdrawalsData <- loadWithdrawalsData
  pure $ (,,) <$> tradesData <*> depositsData <*> withdrawalsData

addTrade :: TradeRow -> Ledger -> Ledger
addTrade TradeRow{..} =
  trade
    (sold tradeSoldAsset)
    (bought tradeBoughtAsset)
    (price tradePrice)
    (amount tradeQuantity)
    (fee tradeFee tradeFeeAsset)

addDeposit :: DepositRow -> Ledger -> Ledger
addDeposit DepositRow{..} =
  deposit depositAsset (amount depositQuantity)

addWithdrawal :: WithdrawalRow -> Ledger -> Ledger
addWithdrawal WithdrawalRow{..} =
  withdraw withdrawalAsset (fee withdrawalFee withdrawalAsset) (amount withdrawalQuantity)

readLedger :: App Ledger
readLedger = do
  eData <- loadData

  case eData of
    Left err -> logError (displayShow err) >> pure startLedger
    Right (trades, deposits, withdrawals) -> pure finalLedger
     where
      ledgerWithTrades = foldl' (flip addTrade) startLedger trades
      ledgerWithTradesAndDeposits = foldl' (flip addDeposit) ledgerWithTrades deposits
      finalLedger = foldl' (flip addWithdrawal) ledgerWithTradesAndDeposits withdrawals