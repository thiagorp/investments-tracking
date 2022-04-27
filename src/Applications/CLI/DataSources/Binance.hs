{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Applications.CLI.DataSources.Binance (readLedger) where

import Applications.CLI.DataSources.Common
import Applications.CLI.Import
import Data.Csv
import Investments.Main
import RIO.ByteString.Lazy qualified as LBS
import RIO.Char (isLetter)
import RIO.Text qualified as T

data Binance

parseAmountAndAsset :: Text -> Parser (Rational, Asset)
parseAmountAndAsset value = do
  quantity <- textToRational (T.takeWhile (not . isLetter) value)
  pure
    ( quantity
    , Asset $ T.drop (T.length value - 3) value
    )

textToRational :: Text -> Parser Rational
textToRational value = toRational @Double <$> parseField (encodeUtf8 (T.filter (/= ',') value))

instance FromNamedRecord (TradeRow Binance) where
  parseNamedRecord row = do
    side <- row .: "Side"
    (executedQuantity, executedAsset) <- row .: "Executed" >>= parseAmountAndAsset
    price_ <- row .: "Price" >>= textToRational
    (amountQuantity, amountAsset) <- row .: "Amount" >>= parseAmountAndAsset
    (tradeFee, tradeFeeAsset) <- row .: "Fee" >>= parseAmountAndAsset

    let (tradeBoughtAsset, tradeSoldAsset, tradeQuantity, tradePrice) =
          if side == ("BUY" :: Text)
            then (executedAsset, amountAsset, executedQuantity, price_)
            else (amountAsset, executedAsset, amountQuantity, 1 / price_)

    pure TradeRow{..}

instance FromNamedRecord (DepositRow Binance) where
  parseNamedRecord row = do
    depositAsset <- Asset <$> row .: "Coin"
    depositQuantity <- row .: "Amount" >>= textToRational
    pure DepositRow{..}

instance FromNamedRecord (WithdrawalRow Binance) where
  parseNamedRecord row = do
    withdrawalAsset <- Asset <$> row .: "Coin"
    withdrawalQuantity <- row .: "Amount" >>= textToRational
    withdrawalFee <- row .: "TransactionFee" >>= textToRational
    pure WithdrawalRow{..}

type TradeRows = Vector (TradeRow Binance)
type DepositRows = Vector (DepositRow Binance)
type WithdrawalRows = Vector (WithdrawalRow Binance)

loadTrades :: App (Either String TradeRows)
loadTrades = do
  dir <- dataDir <$> appSettings
  fileContents <- readFileBinary (dir <> "/binance/trades.csv")
  pure $ snd <$> decodeByName (LBS.fromStrict fileContents)

loadDeposits :: App (Either String DepositRows)
loadDeposits = do
  dir <- dataDir <$> appSettings
  fileContents <- readFileBinary (dir <> "/binance/deposits.csv")
  pure $ snd <$> decodeByName (LBS.fromStrict fileContents)

loadWithdrawals :: App (Either String WithdrawalRows)
loadWithdrawals = do
  dir <- dataDir <$> appSettings
  fileContents <- readFileBinary (dir <> "/binance/withdrawals.csv")
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