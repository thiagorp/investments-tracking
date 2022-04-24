{-# LANGUAGE RecordWildCards #-}

module Investments.Domain (
  Asset (..),
  -- Actions
  startLedger,
  deposit,
  withdraw,
  trade,
  receiveDividend,
  -- Query
  assetsBalance,
  -- Util
  amount,
  fee,
  price,
) where

import Import
import RIO.Map qualified as Map

newtype Asset = Asset Text deriving (Show, Eq, Ord)
newtype AssetPrice = AssetPrice Rational deriving (Show, Eq)
newtype AssetAmount = AssetAmount Rational deriving (Show, Eq)
newtype Fee = Fee Rational deriving (Show, Eq)

amount :: Rational -> AssetAmount
amount = AssetAmount

price :: Rational -> AssetPrice
price = AssetPrice

fee :: Rational -> Fee
fee = Fee

unAssetAmount :: AssetAmount -> Rational
unAssetAmount (AssetAmount assetAmount) = assetAmount

unAssetPrice :: AssetPrice -> Rational
unAssetPrice (AssetPrice p) = p

unFee :: Fee -> Rational
unFee (Fee f) = f

data Deposit = Deposit
  { depositAmount :: AssetAmount
  , depositAsset :: Asset
  }
  deriving (Show, Eq)

data Withdraw = Withdraw
  { withdrawAmount :: AssetAmount
  , withdrawAsset :: Asset
  , withdrawAssetPrice :: AssetPrice
  }
  deriving (Show, Eq)

data Trade = Trade
  { tradeBaseAsset :: Asset
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
deposit depositAsset depositAmount (Ledger entries) =
  Ledger $ entries ++ [LedgerDeposit $ Deposit{..}]

withdraw :: Asset -> AssetPrice -> AssetAmount -> Ledger -> Ledger
withdraw withdrawAsset withdrawAssetPrice withdrawAmount (Ledger entries) =
  Ledger $ entries ++ [LedgerWithdraw $ Withdraw{..}]

trade :: Asset -> Asset -> AssetPrice -> AssetAmount -> Fee -> Ledger -> Ledger
trade tradeBaseAsset tradeBoughtAsset tradeAssetPrice tradeAmount tradeFee (Ledger entries) =
  Ledger $ entries ++ [LedgerTrade $ Trade{..}]

receiveDividend :: Asset -> AssetAmount -> Ledger -> Ledger
receiveDividend dividendAsset dividendAmount (Ledger entries) =
  Ledger $ entries ++ [LedgerDividend $ Dividend{..}]

assetsBalance :: Ledger -> Map Asset Rational
assetsBalance (Ledger entries) = foldl' (flip updateBalances) Map.empty entries
 where
  updateBalances (LedgerDeposit (Deposit{..})) = addAmountToAsset (unAssetAmount depositAmount) depositAsset
  updateBalances (LedgerWithdraw (Withdraw{..})) = subAmountFromAsset (unAssetAmount withdrawAmount) withdrawAsset
  updateBalances (LedgerTrade (Trade{..})) =
    addAmountToAsset (unAssetAmount tradeAmount) tradeBoughtAsset
      . subAmountFromAsset (unAssetAmount tradeAmount * unAssetPrice tradeAssetPrice + unFee tradeFee) tradeBaseAsset
  updateBalances (LedgerDividend (Dividend{..})) = addAmountToAsset (unAssetAmount dividendAmount) dividendAsset

  addAmountToAsset :: Rational -> Asset -> Map Asset Rational -> Map Asset Rational
  addAmountToAsset amounToAdd = Map.alter $ alterFn (+) amounToAdd

  subAmountFromAsset :: Rational -> Asset -> Map Asset Rational -> Map Asset Rational
  subAmountFromAsset amountToSub = Map.alter $ alterFn (-) amountToSub

  alterFn fn amountToChange = Just . flip fn amountToChange . fromMaybe 0
