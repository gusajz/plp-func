module DiccionarioSpec (spec) where

import Test.Hspec

spec :: Spec
spec = do
    describe "Ejercicio 6" $ do
        describe "vacio" $ do
            it "Deberia funcionar" $ 1 `shouldBe` 1

