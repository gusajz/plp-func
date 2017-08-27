import Test.Hspec
import Data.List

import Lib
import Arbol23

main :: IO ()
main = hspec $ do
    describe "Ejercicio 1" $ do
        describe "internos" $ do
            it "Deberia hacer el preorden de arbolito1" $
                (internos arbolito1) `shouldBe`  [0, 1, 2, 3, 4, 5, 6, 7]
            it "Deberia hacer el preorden de arbolito2" $
                (internos arbolito2) `shouldBe`  [True, False, True]

        describe "esHoja" $ do
            it "Deberia dar True con una hoja" $
                (esHoja (Hoja 1)) `shouldBe`  True
            it "Deberia dar False de arbolito1" $
                (esHoja arbolito1) `shouldBe`  False
                -- it "Deberia hacer el preorden de arbolito3" $
            --     (internos arbolito2) `shouldBe`  [True, False, True]
