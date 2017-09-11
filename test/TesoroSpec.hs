module DiccionarioSpec (spec) where

import Diccionario
import Arbol23
import Tesoro

import Test.Hspec
import Data.List

spec :: Spec
spec = do
    describe "Ejercicio 10" $ do
        describe "busquedaDelTesoro" $ do
            it "Deberia hacer algo" $ Nothing `shouldBe` Nothing
        