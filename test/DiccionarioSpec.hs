module DiccionarioSpec (spec) where

import Diccionario
import Arbol23

import Test.Hspec

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
        describe "definir" $ do
            it "Deberia definir como una hoja el primer elemento de un diccionario vacío" $ 
                estructura (definir 0 "hola" diccionarioVacio) `shouldBe` Just (Hoja (0, "hola"))
            it "Deberia definir insertar un elemento en un diccionario no vacío" $ 
                estructura (definir 1 "chau" $ definir 0 "hola" diccionarioVacio) `shouldBe` 
                    Just (insertar 1 "chau" (cmp diccionarioVacio) (Hoja (0, "hola")))
