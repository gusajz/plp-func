module DiccionarioSpec (spec) where

import Diccionario
import Test.Hspec

spec :: Spec
spec = do
    describe "Ejercicio 6" $ do
        describe "vacio" $ do
            it "Deberia tener Nothing en la estructura" $ estructura diccionarioVacio `shouldBe` Nothing
            it "Deberia tener el mismo comparador 1" $ (cmp diccionarioVacio) 1 2 `shouldBe` 1 > 2
            it "Deberia tener el mismo comparador 2" $ (cmp diccionarioVacio) 1 1 `shouldBe` 1 > 1
            it "Deberia tener el mismo comparador 3" $ (cmp diccionarioVacio) 1 0 `shouldBe` 1 > 0
            where 
                diccionarioVacio :: Diccionario Integer Integer 
                diccionarioVacio = (vacio (>))


