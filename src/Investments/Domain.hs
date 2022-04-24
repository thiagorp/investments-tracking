{-# LANGUAGE RecordWildCards #-}

module Investments.Domain (
  Asset (..),
  AssetPrice (..),
  startLedger,
  -- Actions
  deposit,
  withdraw,
  trade,
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

data LedgerEntry
  = LedgerDeposit Deposit
  | LedgerTrade Trade
  | LedgerWithdraw Withdraw
  deriving (Show, Eq)

newtype Ledger = Ledger [LedgerEntry]
  deriving (Show, Eq)

-- Data manipulation

startLedger :: Ledger
startLedger = Ledger []

deposit :: Asset -> AssetPrice -> Rational -> Ledger -> Ledger
deposit asset assetPrice amount (Ledger entries) = Ledger $ entries ++ [depositEntry]
 where
  depositEntry =
    LedgerDeposit $
      Deposit
        { depositAmount = amount
        , depositAsset = asset
        , depositAssetPrice = assetPrice
        }

withdraw :: Asset -> AssetPrice -> Rational -> Ledger -> Ledger
withdraw asset assetPrice amount (Ledger entries) = Ledger $ entries ++ [withdrawEntry]
 where
  withdrawEntry =
    LedgerWithdraw $
      Withdraw
        { withdrawAmount = amount
        , withdrawAsset = asset
        , withdrawAssetPrice = assetPrice
        }

trade :: Asset -> Asset -> AssetPrice -> Rational -> Rational -> Ledger -> Ledger
trade baseAsset boughtAsset assetPrice amount fee (Ledger entries) = Ledger $ entries ++ [tradeEntry]
 where
  tradeEntry =
    LedgerTrade $
      Trade
        { tradeBaseAsset = baseAsset
        , tradeBoughtAsset = boughtAsset
        , tradeAssetPrice = assetPrice
        , tradeAmount = amount
        , tradeFee = fee
        }

assetsBalance :: Ledger -> Map Asset Rational
assetsBalance (Ledger entries) = foldl' (flip updateBalances) Map.empty entries
 where
  updateBalances (LedgerDeposit (Deposit{..})) = Map.alter (alterAdd depositAmount) depositAsset
  updateBalances (LedgerWithdraw (Withdraw{..})) = Map.alter (alterSub withdrawAmount) withdrawAsset
  updateBalances (LedgerTrade (Trade{..})) =
    Map.alter (alterAdd tradeAmount) tradeBoughtAsset
      . Map.alter (alterSub (tradeAmount * unAssetPrice tradeAssetPrice + tradeFee)) tradeBaseAsset

  alterAdd = alterFn (+)
  alterSub = alterFn (-)
  alterFn fn amount = Just . flip fn amount . fromMaybe 0