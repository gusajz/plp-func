module Tesoro 
        ( busquedaDelTesoro
        ) where


import Diccionario
import Arbol23

busquedaDelTesoro :: Eq a => a -> (a -> Bool) -> Diccionario a a -> Maybe a
busquedaDelTesoro = undefined