module TesoroSpec (spec) where

import Diccionario
import Arbol23
import Tesoro

import Test.Hspec
import Data.List

juego1 = definirVarias [("inicio","casa"),("auto","flores"),("calle","auto"),("casa","escalera"),("ropero","alfajor"),("escalera","ropero")] (vacio (<))
juego2 = definirVarias [("leon","elefante"),("cebra","jirafa"),("pato","cebra"),("elefante","pato")] (vacio (<))
juegoVacio :: Diccionario String String 
juegoVacio = vacio (<)

spec :: Spec
spec = do
    describe "Ejercicio 10" $ do
        describe "busquedaDelTesoro sobre un diccionario no vacío" $ do
            it "Deberia encontrar un tesoro" $ (busquedaDelTesoro "calle" (== "flores") juego1) `shouldBe` (Just "flores")
            it "Deberia no encontrar un tesoro" $ (busquedaDelTesoro "calle" (== "escalera") juego1) `shouldBe` Nothing

        describe "busquedaDelTesoro sobre un diccionario vacío" $ do
            it "Deberia no encontrar un tesoro" $ (busquedaDelTesoro "calle" (== "escalera") juego1) `shouldBe` Nothing

        describe "busquedaDelTesoro sobre un diccionario con todos los elementos relacionados recorriendo todos" $ do
            it "Deberia no encontrar un tesoro" $ (busquedaDelTesoro "leon" (== "jirafa") juego2) `shouldBe` (Just "jirafa")
            it "Deberia no encontrar un tesoro" $ (busquedaDelTesoro "leon" (== "cocodrilo") juego2) `shouldBe` Nothing

        describe "busquedaDelTesoro sobre dicc2" $ do
            it "Debería encontrar alfajor" $
                (Just "alfajor") `shouldBe` (busquedaDelTesoro "inicio" ((=='a').head) dicc2)
