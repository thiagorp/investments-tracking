{-# LANGUAGE RecordWildCards #-}

module Applications.CLI.DataSources.Common where

import Applications.CLI.Import
import Investments.Main

data TradeRow exchange = TradeRow
  { tradeBoughtAsset :: Asset
  , tradeSoldAsset :: Asset
  , tradeQuantity :: Rational
  , tradeFee :: Rational
  , tradeFeeAsset :: Asset
  , tradePrice :: Rational
  }
  deriving (Show)

data DepositRow exchange = DepositRow
  { depositAsset :: Asset
  , depositQuantity :: Rational
  }

data WithdrawalRow exchange = WithdrawalRow
  { withdrawalAsset :: Asset
  , withdrawalQuantity :: Rational
  , withdrawalFee :: Rational
  }

addTrade :: TradeRow exchange -> Ledger -> Ledger
addTrade TradeRow{..} =
  trade
    (sold tradeSoldAsset)
    (bought tradeBoughtAsset)
    (price tradePrice)
    (amount tradeQuantity)
    (fee tradeFee tradeFeeAsset)

addDeposit :: DepositRow exchange -> Ledger -> Ledger
addDeposit DepositRow{..} =
  deposit
    depositAsset
    (amount depositQuantity)

addWithdrawal :: WithdrawalRow exchange -> Ledger -> Ledger
addWithdrawal WithdrawalRow{..} =
  withdraw
    withdrawalAsset
    (fee withdrawalFee withdrawalAsset)
    (amount withdrawalQuantity)

addTrades :: Foldable t => t (TradeRow exchange) -> Ledger -> Ledger
addTrades = flip $ foldl' (flip addTrade)

addDeposits :: Foldable t => t (DepositRow exchange) -> Ledger -> Ledger
addDeposits = flip $ foldl' (flip addDeposit)

addWithdrawals :: Foldable t => t (WithdrawalRow exchange) -> Ledger -> Ledger
addWithdrawals = flip $ foldl' (flip addWithdrawal)