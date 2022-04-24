module InvestmentsSpec (spec) where

import Investments.Domain
import RIO
import RIO.Map qualified as Map
import Test.Hspec

usd :: Asset
usd = Asset "USD"

cad :: Asset
cad = Asset "CAD"

btc :: Asset
btc = Asset "BTC"

spec :: Spec
spec = do
  assetsBalanceSpec

assetsBalanceSpec :: Spec
assetsBalanceSpec =
  describe "assetsBalance" $ do
    it "works with a single deposit" $ do
      let ledger =
            startLedger
              & deposit usd (amount 1000)

      let balance = assetsBalance ledger

      balance `shouldBe` Map.fromList [(usd, 1000)]

    it "works with multiple deposits of the same currency" $ do
      let ledger =
            startLedger
              & deposit usd (amount 1000)
              & deposit usd (amount 1500)
              & deposit usd (amount 700)

      let balance = assetsBalance ledger

      balance `shouldBe` Map.fromList [(usd, 3200)]

    it "works with multiple currencies" $ do
      let ledger =
            startLedger
              & deposit usd (amount 1000)
              & deposit cad (amount 1500)

      let balance = assetsBalance ledger

      balance `shouldBe` Map.fromList [(usd, 1000), (cad, 1500)]

    it "works with deposits, trades and withdrawals" $ do
      let ledger =
            startLedger
              & deposit usd (amount 1000)
              & deposit usd (amount 1500)
              & trade usd btc (price 1000) (amount 1.2) (fee 0)
              & deposit cad (amount 500)

      let balance = assetsBalance ledger

      balance `shouldBe` Map.fromList [(usd, 1300), (cad, 500), (btc, 1.2)]

    it "withdraws the trade fee directly from the base asset" $ do
      let ledger =
            startLedger
              & deposit usd (amount 1000)
              & trade usd btc (price 500) (amount 1) (fee 10)

      let balance = assetsBalance ledger

      balance `shouldBe` Map.fromList [(usd, 490), (btc, 1)]

    it "adds dividends to the amount of its assets" $ do
      let ledger =
            startLedger
              & deposit usd (amount 1000)
              & deposit usd (amount 5000)
              & trade usd btc (price 500) (amount 1) (fee 10)
              & receiveDividend btc (amount 0.01)

      let balance = assetsBalance ledger

      Map.lookup btc balance `shouldBe` Just 1.01
