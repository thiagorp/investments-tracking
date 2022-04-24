{-# LANGUAGE RecordWildCards #-}

module Investments.Domain (
  Asset (..),
  AssetPrice (..),
  startLedger,
  -- Actions
  deposit,
  withdraw,
  trade,
  receiveDividend,
  -- Query
  assetsBalance,
) where

import Import
import RIO.Map qualified as Map

newtype Asset = Asset Text deriving (Show, Eq, Ord)
newtype AssetPrice = AssetPrice Rational deriving (Show, Eq)

unAssetPrice :: AssetPrice -> Rational
unAssetPrice (AssetPrice price) = price

data Deposit = Deposit
  { depositAmount :: Rational
  , depositAsset :: Asset
  , depositAssetPrice :: AssetPrice
  }
  deriving (Show, Eq)

data Withdraw = Withdraw
  { withdrawAmount :: Rational
  , withdrawAsset :: Asset
  , withdrawAssetPrice :: AssetPrice
  }
  deriving (Show, Eq)

data Trade = Trade
  { tradeBaseAsset :: Asset
  , tradeBoughtAsset :: Asset
  , tradeAssetPrice :: AssetPrice
  , tradeAmount :: Rational
  , tradeFee :: Rational
  }
  deriving (Show, Eq)

data Dividend = Dividend
  { dividendAsset :: Asset
  , dividendAmount :: Rational
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

deposit :: Asset -> AssetPrice -> Rational -> Ledger -> Ledger
deposit depositAsset depositAssetPrice depositAmount (Ledger entries) =
  Ledger $ entries ++ [LedgerDeposit $ Deposit{..}]

withdraw :: Asset -> AssetPrice -> Rational -> Ledger -> Ledger
withdraw withdrawAsset withdrawAssetPrice withdrawAmount (Ledger entries) =
  Ledger $ entries ++ [LedgerWithdraw $ Withdraw{..}]

trade :: Asset -> Asset -> AssetPrice -> Rational -> Rational -> Ledger -> Ledger
trade tradeBaseAsset tradeBoughtAsset tradeAssetPrice tradeAmount tradeFee (Ledger entries) =
  Ledger $ entries ++ [LedgerTrade $ Trade{..}]

receiveDividend :: Asset -> Rational -> Ledger -> Ledger
receiveDividend dividendAsset dividendAmount (Ledger entries) =
  Ledger $ entries ++ [LedgerDividend $ Dividend{..}]

assetsBalance :: Ledger -> Map Asset Rational
assetsBalance (Ledger entries) = foldl' (flip updateBalances) Map.empty entries
 where
  updateBalances (LedgerDeposit (Deposit{..})) = addAmountToAsset depositAmount depositAsset
  updateBalances (LedgerWithdraw (Withdraw{..})) = subAmountFromAsset withdrawAmount withdrawAsset
  updateBalances (LedgerTrade (Trade{..})) =
    addAmountToAsset tradeAmount tradeBoughtAsset
      . subAmountFromAsset (tradeAmount * unAssetPrice tradeAssetPrice + tradeFee) tradeBaseAsset
  updateBalances (LedgerDividend (Dividend{..})) = addAmountToAsset dividendAmount dividendAsset

  addAmountToAsset :: Rational -> Asset -> Map Asset Rational -> Map Asset Rational
  addAmountToAsset amount = Map.alter $ alterFn (+) amount

  subAmountFromAsset :: Rational -> Asset -> Map Asset Rational -> Map Asset Rational
  subAmountFromAsset amount = Map.alter $ alterFn (-) amount

  alterFn fn amount = Just . flip fn amount . fromMaybe 0
