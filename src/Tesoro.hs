module Tesoro 
        ( busquedaDelTesoro
        ) where


import Diccionario
import Arbol23

import Debug.Trace

busquedaDelTesoro :: (Show a, Eq a) => a -> (a -> Bool) -> Diccionario a a -> Maybe a
busquedaDelTesoro pista validador dicc = last (takeWhile noEncontrado pistas) >>= flip obtener dicc
    where 
        pistas = iterate obtenerPista (Just pista)
        
        obtenerPista (Just p) = obtener p dicc
        obtenerPista Nothing = Nothing

        noEncontrado Nothing = False
        noEncontrado (Just p) = not (validador p)
        
