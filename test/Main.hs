module Main (main) where

import RIO
import Spec qualified
import Test.Hspec.Formatters
import Test.Hspec.Runner

main :: IO ()
main = do
  hspecWith defaultConfig {configFormatter = Just progress} Spec.spec
