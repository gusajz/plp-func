import Test.Hspec
import Data.List

import Lib
import Arbol23

main :: IO ()
main = hspec $ do
    describe "Ejercicio 2" $ do
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

    describe "Ejercicio 3" $ do
        describe "mapA23" $ do
            it "Debería hacer map con las dos funciones identidad" $
                (mapA23 id id) arbolito1 `shouldBe` arbolito1

            it "Debería hacer map con las dos funciones" $
                (mapA23 (:"A") (+1)) arbolito1 `shouldBe` Tres 1 2 (Dos 3 (Hoja "aA") (Hoja "bA"))
                            (Tres 4 5 (Hoja "cA") (Hoja "dA") (Dos 6 (Hoja "eA") (Hoja "fA")))
                            (Dos 7 (Hoja "gA") (Dos 8 (Hoja "hA") (Hoja "iA")))
