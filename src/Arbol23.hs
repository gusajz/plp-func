module Arbol23 
        ( Arbol23 (Hoja, Dos, Tres)
        , foldA23
        , mapA23
        , hojas
        , internos
        , esHoja
        , arbolito1
        , arbolito2
        ) where

import Data.Char
data Arbol23 a b = Hoja a | Dos b (Arbol23 a b) (Arbol23 a b) | Tres b b (Arbol23 a b) (Arbol23 a b) (Arbol23 a b)

{- Funciones para mostrar el árbol. -}

instance (Show a, Show b) => Show (Arbol23 a b) where
    show = ("\n" ++) . (padTree 0 0 False)


instance (Eq a, Eq b) => Eq (Arbol23 a b) where
    Hoja x1 == Hoja x2 = x1 == x2
    Dos x1 a1 b1 == Dos x2 a2 b2 = x1 == x2 && a1 == a2 && b1 == b2
    Tres x1 y1 a1 b1 c1 == Tres x2 y2 a2 b2 c2 = x1 == x2 && y1 == y2 && a1 == a2 && b1 == b2 && c1 == c2

padlength = 5    
    
padTree:: (Show a, Show b) => Int -> Int -> Bool -> (Arbol23 a b)-> String
padTree nivel acum doPad t = case t of 
                                  (Hoja x) -> initialPad ++ stuff x
                                  (Dos x i d) -> initialPad ++ stuff x ++ 
                                                 pad padlength ++ rec x False i ++ "\n" ++
                                                 rec x True d ++ "\n"
                                  (Tres x y i m d) -> initialPad ++ stuff x ++ --(' ':tail (stuff y)) ++
                                                      pad padlength ++ rec x False i ++ "\n" ++
                                                      pad levelPad ++ stuff y ++ pad padlength ++ rec x False m ++ "\n" ++
                                                      rec x True d ++ "\n" 
  where l = length . stuff
        levelPad = (padlength*nivel + acum)
        initialPad = (if doPad then pad levelPad else "")
        rec x = padTree (nivel+1) (acum+l x)
            
stuff:: Show a => a -> String
stuff x = if n > l then pad (n-l) ++ s else s
  where s = show x
        l = length s
        n = padlength

pad:: Int -> String
pad i = replicate i ' '

{- Funciones pedidas. -}

foldA23 :: (a -> c) -> (b -> c -> c -> c) -> (b -> b -> c -> c -> c -> c) -> Arbol23 a b -> c
foldA23 f1 f2 f3 (Hoja x) = f1 x
foldA23 f1 f2 f3 (Dos x ab1 ab2) = f2 x (foldA23 f1 f2 f3 ab1) (foldA23 f1 f2 f3 ab2)
foldA23 f1 f2 f3 (Tres x y ab1 ab2 ab3) = f3 x y (foldA23 f1 f2 f3 ab1) (foldA23 f1 f2 f3 ab2) (foldA23 f1 f2 f3 ab3)
    

--Lista en preorden de los internos del árbol.
internos :: Arbol23 a b -> [b]
internos = foldA23 (const []) f2 f3
    where 
        f2 x acc1 acc2 = [x] ++ acc1 ++ acc2
        f3 x y acc1 acc2 acc3 = [x] ++ [y] ++ acc1 ++ acc2 ++ acc3

--Lista las hojas de izquierda a derecha.
hojas :: Arbol23 a b -> [a]
hojas = foldA23 f1 f2 f3
    where 
        f1 x = [x]
        f2 _ acc1 acc2 = acc1 ++ acc2
        f3 _ _ acc1 acc2 acc3 = acc1 ++ acc2 ++ acc3

esHoja :: Arbol23 a b -> Bool
esHoja a = case a of 
                        (Hoja _) -> True
                        _ -> False


mapA23:: (a -> c) -> (b -> d) -> Arbol23 a b-> Arbol23 c d
mapA23 f1 f2  = foldA23 (\x -> Hoja (f1 x) ) (\x ab1 ab2 -> Dos (f2 x) ab1 ab2 ) (\x y ab1 ab2 ab3 -> Tres (f2 x) (f2 y) ab1 ab2 ab3  )
    --where 
        --fa x = Hoja (f1 x)
        --fb x ab1 ab2 = Dos ( (f2 x) ab1 ab2 )
        --fc x y ab1 ab2 ab3 = Tres ( (f2 x) (f2 y) ab1 ab2 ab3 )

--mapA23 f1 f2 (Hoja x) = Hoja (f1 x)
--mapA23 f1 f2 (Dos x ab1 ab2) = Dos (f2 x) (mapA23 f1 f2 ab1) (mapA23 f1 f2 ab2)
--mapA23 f1 f2 (Tres x y ab1 ab2 ab3) = Tres (f2 x) (f2 y) (mapA23 f1 f2 ab1) (mapA23 f1 f2 ab2) (mapA23 f1 f2 ab3)

--Ejemplo de uso de mapA23.
--Incrementa en 1 el valor de las hojas.

incrementarHojas::Num a =>Arbol23 a b->Arbol23 a b
incrementarHojas = mapA23 (+1) id


--Trunca el árbol hasta un determinado nivel. Cuando llega a 0, reemplaza el resto del árbol por una hoja con el valor indicado.
--Funciona para árboles infinitos.
truncar::a->Integer->Arbol23 a b->Arbol23 a b
truncar = undefined

--Evalúa las funciones tomando los valores de los hijos como argumentos.
--En el caso de que haya 3 hijos, asocia a izquierda.
evaluar::Arbol23 a (a->a->a)->a
evaluar = undefined

--Ejemplo:
--evaluar (truncar 0 6 arbolito3) = 22 = (1*2-3)+(2*3-4)+(3*4-5)+(4*5-6)

{- Árboles de ejemplo. -}
arbolito1::Arbol23 Char Int
arbolito1 = Tres 0 1
        (Dos 2 (Hoja 'a') (Hoja 'b'))
        (Tres 3 4 (Hoja 'c') (Hoja 'd') (Dos 5 (Hoja 'e') (Hoja 'f')))
        (Dos 6 (Hoja 'g') (Dos 7 (Hoja 'h') (Hoja 'i')))

arbolito2::Arbol23 Int Bool
arbolito2 = Dos True (Hoja (-1)) (Tres False True (Hoja 0) (Hoja (-2)) (Hoja 4))

arbolito3::Arbol23 Int (Int->Int->Int)
arbolito3 = Dos (+) (Tres (*) (-) (Hoja 1) (Hoja 2) (Hoja 3)) (incrementarHojas arbolito3)

arbolito4::Arbol23 Int Char
arbolito4 = Dos 'p' (Dos 'l' (Dos 'g' (Hoja 5) (Hoja 2)) (Tres 'r' 'a' (Hoja 0)(Hoja 1)(Hoja 12))) 
                    (Dos 'p' (Tres 'n' 'd' (Hoja (-3))(Hoja 4)(Hoja 9)) (Dos 'e' (Hoja 20)(Hoja 7)))

