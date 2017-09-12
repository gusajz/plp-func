module Tesoro 
        ( busquedaDelTesoro
        ) where


import Diccionario
import Arbol23

import Debug.Trace

busquedaDelTesoro :: (Show a, Eq a) => a -> (a -> Bool) -> Diccionario a a -> Maybe a
busquedaDelTesoro pista esTesoro dicc = last (takeWhile noEncontrado pistas) >>= flip obtener dicc
    where 
        -- Usamos =<< porque obtener debería recibir "a", no Maybe a.
        pistas = iterate (flip obtener dicc =<<) (Just pista)
        -- Otra opción:
        -- obtenerPista (Just p) = flip obtener dicc p
        -- obtenerPista Nothing = Nothing

        -- El tesoro no se encontró si hay un Nothing o hay un Just, pero el contenido no es el tesoro.
        noEncontrado = maybe False (not . esTesoro)
        
