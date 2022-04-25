module InvestmentsSpec (spec) where

import Investments.Domain
import RIO
import RIO.Map qualified as Map
import RIO.Set qualified as Set
import Test.Hspec

usd :: Asset
usd = Asset "USD"

cad :: Asset
cad = Asset "CAD"

btc :: Asset
btc = Asset "BTC"

eth :: Asset
eth = Asset "ETH"

spec :: Spec
spec = do
  assetsBalanceSpec
  ledgerValueSpec

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
              & trade (sold usd) (bought btc) (price 1000) (amount 1.2) (fee 0 usd)
              & deposit cad (amount 500)
              & withdraw usd (fee 10 usd) (amount 1000)

      let balance = assetsBalance ledger

      balance `shouldBe` Map.fromList [(usd, 290), (cad, 500), (btc, 1.2)]

    it "withdraws the trade fee from the correct asset" $ do
      let ledger =
            startLedger
              & deposit usd (amount 1000)
              & trade (sold usd) (bought btc) (price 500) (amount 1) (fee 10 usd)
              & trade (sold usd) (bought btc) (price 500) (amount 0.5) (fee 0.05 btc)

      let balance = assetsBalance ledger

      balance `shouldBe` Map.fromList [(usd, 240), (btc, 1.45)]

    it "adds dividends to the amount of its assets" $ do
      let ledger =
            startLedger
              & deposit usd (amount 1000)
              & deposit usd (amount 5000)
              & trade (sold usd) (bought btc) (price 500) (amount 1) (fee 10 usd)
              & receiveDividend btc (amount 0.01)

      let balance = assetsBalance ledger

      Map.lookup btc balance `shouldBe` Just 1.01

ledgerValueSpec :: Spec
ledgerValueSpec =
  describe "ledgerValue" $ do
    it "works with only deposits from the same currency" $ do
      let ledger =
            startLedger
              & deposit usd (amount 1000)
              & deposit usd (amount 1400)

      let value = ledgerValue usd Map.empty ledger

      value `shouldBe` LedgerValueSuccess 2400

    it "considers the conversion rate for deposits from other currencies" $ do
      let ledger =
            startLedger
              & deposit usd (amount 1000)
              & deposit cad (amount 1500)

          conversionRates = Map.fromList [(cad, price 0.75)]

      let value = ledgerValue usd conversionRates ledger

      value `shouldBe` LedgerValueSuccess (1000 + 1500 * 0.75)

    it "fails if one conversion rate is missing" $ do
      let ledger =
            startLedger
              & deposit usd (amount 1000)
              & deposit cad (amount 1500)

          conversionRates = Map.empty

      let value = ledgerValue usd conversionRates ledger

      value `shouldBe` ConversionRatesMissing (Set.singleton cad)

    it "fails with all missing assets if more than conversion rate is missing" $ do
      let ledger =
            startLedger
              & deposit usd (amount 1000)
              & deposit btc (amount 0.45)
              & deposit cad (amount 1500)
              & deposit eth (amount 4.5)

          conversionRates = Map.fromList [(cad, price 0.8)]

      let value = ledgerValue usd conversionRates ledger

      value `shouldBe` ConversionRatesMissing (Set.fromList [btc, eth])

    it "works with a more complex ledger" $ do
      let ledger =
            startLedger
              & deposit usd (amount 10000)
              & deposit cad (amount 1500)
              & trade (sold usd) (bought btc) (price 2000) (amount 0.5) (fee 5 usd)
              & withdraw cad (fee 10 usd) (amount 750)
              & withdraw cad (fee 10 cad) (amount 300)
              & trade (sold btc) (bought eth) (price 0.8) (amount 0.5) (fee 3 usd)
              & deposit cad (amount 500)
              & deposit btc (amount 0.3)
              & withdraw eth (fee 0.01 eth) (amount 0.3)

          conversionRates =
            Map.fromList
              [ (cad, price 0.8)
              , (eth, price 450)
              , (btc, price 1000)
              ]

      let value = ledgerValue usd conversionRates ledger

      -- USD: 10,000 - 1,000 - 5 - 10 - 3 = 8,982
      -- CAD: 1,500 - 750 - 10 - 300 + 500 = 940
      -- BTC: 0.5 - 0.4 + 0.3 = 0.4
      -- ETH: 0.5 - 0.01 - 0.3 = 0.19
      value `shouldBe` LedgerValueSuccess (8982 + 940 * 0.8 + 0.4 * 1000 + 0.19 * 450)