{-# LANGUAGE RecordWildCards #-}

module Investments.Domain (
  Asset (..),
  LedgerValueResponse (..),
  -- Actions
  startLedger,
  deposit,
  withdraw,
  trade,
  receiveDividend,
  -- Query
  assetsBalance,
  ledgerValue,
  -- Util
  amount,
  bought,
  fee,
  price,
  sold,
) where

import Import
import RIO.Map qualified as Map
import RIO.Set qualified as Set

newtype Asset = Asset Text deriving (Show, Eq, Ord)
newtype AssetPrice = AssetPrice Rational deriving (Show, Eq)
newtype AssetAmount = AssetAmount Rational deriving (Show, Eq)
newtype BoughtAsset = BoughtAsset Asset deriving (Show, Eq)
data Fee = Fee Rational Asset deriving (Show, Eq)
newtype SoldAsset = SoldAsset Asset deriving (Show, Eq)

amount :: Rational -> AssetAmount
amount = AssetAmount

bought :: Asset -> BoughtAsset
bought = BoughtAsset

price :: Rational -> AssetPrice
price = AssetPrice

fee :: Rational -> Asset -> Fee
fee = Fee

sold :: Asset -> SoldAsset
sold = SoldAsset

unAssetAmount :: AssetAmount -> Rational
unAssetAmount (AssetAmount assetAmount) = assetAmount

unAssetPrice :: AssetPrice -> Rational
unAssetPrice (AssetPrice p) = p

feeAmount :: Fee -> Rational
feeAmount (Fee f _) = f

feeAsset :: Fee -> Asset
feeAsset (Fee _ a) = a

data Deposit = Deposit
  { depositAmount :: AssetAmount
  , depositAsset :: Asset
  }
  deriving (Show, Eq)

data Withdraw = Withdraw
  { withdrawAmount :: AssetAmount
  , withdrawAsset :: Asset
  , withdrawFee :: Fee
  }
  deriving (Show, Eq)

data Trade = Trade
  { tradeSoldAsset :: Asset
  , tradeBoughtAsset :: Asset
  , tradeAssetPrice :: AssetPrice
  , tradeAmount :: AssetAmount
  , tradeFee :: Fee
  }
  deriving (Show, Eq)

data Dividend = Dividend
  { dividendAsset :: Asset
  , dividendAmount :: AssetAmount
  }
  deriving (Show, Eq)

data LedgerEntry
  = LedgerDeposit Deposit
  | LedgerTrade Trade
  | LedgerWithdraw Withdraw
  | LedgerDividend Dividend
  deriving (Show, Eq)

newtype Ledger = Ledger [LedgerEntry]
  deriving (Show, Eq)

-- Data manipulation

startLedger :: Ledger
startLedger = Ledger []

deposit :: Asset -> AssetAmount -> Ledger -> Ledger
deposit depositAsset depositAmount =
  addEntry $ LedgerDeposit Deposit{..}

withdraw :: Asset -> Fee -> AssetAmount -> Ledger -> Ledger
withdraw withdrawAsset withdrawFee withdrawAmount =
  addEntry $ LedgerWithdraw Withdraw{..}

trade :: SoldAsset -> BoughtAsset -> AssetPrice -> AssetAmount -> Fee -> Ledger -> Ledger
trade (SoldAsset tradeSoldAsset) (BoughtAsset tradeBoughtAsset) tradeAssetPrice tradeAmount tradeFee =
  addEntry $ LedgerTrade Trade{..}

receiveDividend :: Asset -> AssetAmount -> Ledger -> Ledger
receiveDividend dividendAsset dividendAmount =
  addEntry $ LedgerDividend Dividend{..}

addEntry :: LedgerEntry -> Ledger -> Ledger
addEntry entry (Ledger entries) = Ledger $ entry : entries

-- Queries

assetsBalance :: Ledger -> Map Asset Rational
assetsBalance (Ledger entries) = foldl' (flip updateBalances) Map.empty entries
 where
  updateBalances (LedgerDeposit (Deposit{..})) = addAmountToAsset (unAssetAmount depositAmount) depositAsset
  updateBalances (LedgerWithdraw (Withdraw{..})) =
    subAmountFromAsset (unAssetAmount withdrawAmount) withdrawAsset
      . subAmountFromAsset (feeAmount withdrawFee) (feeAsset withdrawFee)
  updateBalances (LedgerTrade (Trade{..})) =
    addAmountToAsset (unAssetAmount tradeAmount) tradeBoughtAsset
      . subAmountFromAsset (unAssetAmount tradeAmount * unAssetPrice tradeAssetPrice) tradeSoldAsset
      . subAmountFromAsset (feeAmount tradeFee) (feeAsset tradeFee)
  updateBalances (LedgerDividend (Dividend{..})) = addAmountToAsset (unAssetAmount dividendAmount) dividendAsset

  addAmountToAsset :: Rational -> Asset -> Map Asset Rational -> Map Asset Rational
  addAmountToAsset amounToAdd = Map.alter $ alterFn (+) amounToAdd

  subAmountFromAsset :: Rational -> Asset -> Map Asset Rational -> Map Asset Rational
  subAmountFromAsset amountToSub = Map.alter $ alterFn (-) amountToSub

  alterFn fn amountToChange = Just . flip fn amountToChange . fromMaybe 0

data LedgerValueResponse
  = LedgerValueSuccess Rational
  | ConversionRatesMissing (Set Asset)
  deriving (Show, Eq)

instance Semigroup LedgerValueResponse where
  LedgerValueSuccess value1 <> LedgerValueSuccess value2 = LedgerValueSuccess $ value1 + value2
  ConversionRatesMissing missing1 <> ConversionRatesMissing missing2 = ConversionRatesMissing $ missing1 <> missing2
  ConversionRatesMissing missing <> _ = ConversionRatesMissing missing
  _ <> ConversionRatesMissing missing = ConversionRatesMissing missing

ledgerValue :: Asset -> Map Asset AssetPrice -> Ledger -> LedgerValueResponse
ledgerValue baseAsset conversionRates =
  foldl' (<>) (LedgerValueSuccess 0) . Map.elems . Map.mapWithKey convertAsset . assetsBalance
 where
  convertAsset :: Asset -> Rational -> LedgerValueResponse
  convertAsset asset assetAmount =
    if asset == baseAsset
      then LedgerValueSuccess assetAmount
      else case Map.lookup asset conversionRates of
        Just conversionRate ->
          LedgerValueSuccess $ assetAmount * unAssetPrice conversionRate
        Nothing ->
          ConversionRatesMissing $ Set.singleton asset