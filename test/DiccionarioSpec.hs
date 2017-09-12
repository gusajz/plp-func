module DiccionarioSpec (spec) where

import Diccionario
import Arbol23

import Test.Hspec
import Data.List

mismosElementos :: Eq a => [a] -> [a] -> Bool
mismosElementos xs ys = null (xs \\ ys) && null (ys \\ xs)

diccionarioVacio :: Diccionario Integer String 
diccionarioVacio = (vacio (>))

spec :: Spec
spec = do
    describe "Ejercicio 6" $ do
        describe "vacio" $ do
            it "Deberia tener Nothing en la estructura" $ estructura diccionarioVacio `shouldBe` Nothing
            it "Deberia tener el mismo comparador 1" $ (cmp diccionarioVacio) 1 2 `shouldBe` 1 > 2
            it "Deberia tener el mismo comparador 2" $ (cmp diccionarioVacio) 1 1 `shouldBe` 1 > 1
            it "Deberia tener el mismo comparador 3" $ (cmp diccionarioVacio) 1 0 `shouldBe` 1 > 0

    describe "Ejercicio 7" $ do
        describe "definir" $ do
            it "Deberia definir como una hoja el primer elemento de un diccionario vacío" $ 
                estructura (definir 0 "hola" diccionarioVacio) `shouldBe` Just (Hoja (0, "hola"))
            it "Deberia definir insertar un elemento en un diccionario no vacío" $ 
                estructura (definir 1 "chau" $ definir 0 "hola" diccionarioVacio) `shouldBe` 
                    Just (insertar 1 "chau" (cmp diccionarioVacio) (Hoja (0, "hola")))

    describe "Ejercicio 8" $ do
        describe "obtener" $ do
            it "Deberia obtener un valor" $ 
                (obtener 0 dicc1) `shouldBe` (Just "Hola")

            it "Deberia obtener otro valor" $ 
                (obtener 9 dicc1) `shouldBe` (Just "a")

            it "Deberia no obtener una clave que no existe" $ 
                (obtener 533 dicc1) `shouldBe` Nothing


    describe "Ejercicio 9" $ do
        describe "claves" $ do
            it "Debería dar las claves de dicc1" $
                mismosElementos (claves dicc1) [0, -10, 15, 2, 9] `shouldBe` True

            it "Debería dar las claves de dicc2" $
                mismosElementos (claves dicc2) ["inicio", "auto", "calle", "casa", "ropero", "escalera"] `shouldBe` True

            it "Debería dar las claves de dicc3" $
                mismosElementos (claves dicc3) [0, -10, 15, 2, 9] `shouldBe` True