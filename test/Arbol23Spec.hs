module Arbol23Spec (spec) where

import Test.Hspec
import Data.List

import Lib
import Arbol23

arbolGrande =
    Tres 0 1
    (Dos 2
        (Hoja 'a')
        (Hoja 'b')
    ) (Tres 3 4
        (Hoja 'c')
        (Hoja 'd')
        (Dos 5
            (Hoja 'e')
            (Hoja 'f')
        )
      ) (Dos 6
        (Hoja 'g')
        (Dos 7
            (Hoja 'h')
            (Hoja 'i')
        )
        )
arbolEjemploTruncar1 = Dos 'p'
    (Dos 'l'
        (Dos 'g'
            (Hoja 5)
            (Hoja 2)
        )
        (Tres 'r' 'a'
            (Hoja 0)
            (Hoja 1)
            (Hoja 12)
        )
    )
    (Dos 'p'
        (Tres 'n' 'd'
            (Hoja (-3))
            (Hoja 4)
            (Hoja 9)
        )
        (Dos 'e'
            (Hoja 20)
            (Hoja 7)
        )
    )

arbolEjemploTruncar2 = Dos "(+)"
        (Tres "(*)" "(-)"
            (Hoja 1)
            (Hoja 2)
            (Hoja 3)
        )
        (Dos "(+)"
            (Tres "(*)" "(-)"
                (Hoja 2)
                (Hoja 3)
                (Hoja 4)
            )
            (Dos "(+)"
                (Tres "(*)" "(-)"
                    (Hoja 3)
                    (Hoja 4)
                    (Hoja 5)
                )
                (Tres "(*)" "(-)"
                    (Hoja 3)
                    (Hoja 4)
                    (Hoja 5)
                )
            )
        )

arbolTrivial1 = Dos (+)
                    (Hoja 1)
                    (Hoja 1)

arbolTrivial2 = Dos (-)
                    (Hoja 1)
                    (Hoja 1)

arbolGrande2=  Dos (+)
    (Tres (+) (-)
        (Dos (+)
            (Hoja 1)
            (Hoja 1)
        )
        (Dos (-)
            (Hoja 1)
            (Hoja 1)
        )
        (Dos (-)
            (Hoja 0)
            (Hoja 0)
        )
    )
    (Tres (+) (-)
        (Dos (+)
            (Hoja 1)
            (Hoja 1)
        )
        (Dos (+)
            (Hoja 1)
            (Hoja 1)
        )
        (Dos (-)
            (Hoja 0)
            (Hoja 0)
        )
    )


arbolEjemploEvaluar :: Int -> Arbol23 Int (Int -> Int -> Int)
arbolEjemploEvaluar n = Dos (+)
        (Tres (*) (-)
            (Hoja (n + 1))
            (Hoja (n + 2))
            (Hoja (n + 3))
        ) (arbolEjemploEvaluar (n + 1))

arbolEjemploEvaluar1 = arbolEjemploEvaluar 0

spec :: Spec
spec = do
    describe "Ejercicio 2" $ do
        describe "hojas" $ do
            it "Debería obtener las hojas de arbolito1" $
                "abcdefghi" `shouldBe` (hojas arbolito1)
            it "Debería obtener las 10 primeras hojas de arbolito3" $
                [1,2,3,2,3,4,3,4,5,4] `shouldBe` (take 10 (hojas arbolito3))
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
            it "Deberia hacer el preorden de arbolito3" $
                (internos arbolito2) `shouldBe`  [True, False, True]

    describe "Ejercicio 3" $ do
        describe "mapA23" $ do
            it "Debería hacer map con las dos funciones identidad" $
                (mapA23 id id) arbolito1 `shouldBe` arbolito1

            it "Debería hacer map con las dos funciones" $
                (mapA23 (:"A") (+1)) arbolito1 `shouldBe` Tres 1 2 (Dos 3 (Hoja "aA") (Hoja "bA"))
                            (Tres 4 5 (Hoja "cA") (Hoja "dA") (Dos 6 (Hoja "eA") (Hoja "fA")))
                            (Dos 7 (Hoja "gA") (Dos 8 (Hoja "hA") (Hoja "iA")))

            it "Debería incrementarHojas" $
                [0, 1, -1, 5] `shouldBe` (hojas (incrementarHojas arbolito2))


    describe "Ejercicio 4" $ do
        describe "truncar" $ do
            it "Debería obtener las seis primeras hojas de arbolito3" $
                [1,2,3,2,3,4,3,4,5,4,5,6,0,0,0,0,0] `shouldBe` hojas (truncar 0 6 arbolito3)

            it "Hasta el nivel 1, es la raíz original, cambiando los hijos por hojas." $
                (truncar 'b' 1 arbolGrande) `shouldBe` (Tres 0 1) (Hoja 'b') (Hoja 'b') (Hoja 'b')
            it "El resultado de truncar un árbol hasta el nivel 0 es sólo una hoja" $
                (truncar 'a' 0 arbolGrande) `shouldBe` (Hoja 'a')
            it "Ejemplo 1" $
                (truncar 0 3  arbolEjemploTruncar1) `shouldBe` Dos 'p'
                        (Dos 'l'
                            (Dos 'g'
                                (Hoja 0)
                                (Hoja 0)
                            )
                            (Tres 'r' 'a'
                                (Hoja 0)
                                (Hoja 0)
                                (Hoja 0)
                            )
                        )
                        (Dos 'p'
                            (Tres 'n' 'd'
                                (Hoja 0)
                                (Hoja 0)
                                (Hoja 0)
                            )
                            (Dos 'e'
                                (Hoja 0)
                                (Hoja 0)
                            )
                        )
            it "Ejemplo 2" $
                (truncar 0 3  arbolEjemploTruncar2) `shouldBe` Dos "(+)"
                            (Tres "(*)" "(-)"
                                (Hoja 1)
                                (Hoja 2)
                                (Hoja 3)
                            )
                            (Dos "(+)"
                                (Tres "(*)" "(-)"
                                    (Hoja 0)
                                    (Hoja 0)
                                    (Hoja 0)
                                )
                                (Dos "(+)"
                                    (Hoja 0)
                                    (Hoja 0)
                                )
                            )

    describe "Ejercicio 5" $ do
        describe "evaluar" $ do
            it "Debería evaluar las seis primeras hojas de arbolito3" $
                22 `shouldBe` (evaluar (truncar 0 6 arbolito3))
            it "Debería resolver el primer ejemplo de la guia" $
                evaluar (truncar 0 3 arbolEjemploEvaluar1) `shouldBe` -1
            it "Debería ver si el primer ejemplo no da otro numero" $
                evaluar (truncar 0 3 arbolEjemploEvaluar1) `shouldNotBe` 3
            it "Debería resolver el segundo ejemplo de la guia" $
                evaluar (truncar 0 4 arbolEjemploEvaluar1) `shouldBe` 1
            it "Debería checkear que el segundo ejemplo no de otro numero" $
                evaluar (truncar 0 4 arbolEjemploEvaluar1) `shouldNotBe` 0
            it "Debería resolver el tercer ejemplo de la guia" $
                evaluar (truncar 0 5 arbolEjemploEvaluar1) `shouldBe` 8
            it "Debería checkear que el tercer ejemplo no de otro numero" $
                evaluar (truncar 0 5 arbolEjemploEvaluar1) `shouldNotBe` 1
            it "Debería resolver el cuarto ejemplo de la guia" $
                evaluar (truncar 0 6 arbolEjemploEvaluar1) `shouldBe` 22
            it "Debería checkear que el cuarto ejemplo no de otro numero" $
                evaluar (truncar 0 6 arbolEjemploEvaluar1) `shouldNotBe` 20

            it "Debería checkear que evaluar al arboltrivial1 de 2" $
                evaluar arbolTrivial1 `shouldBe` 2

            it "Debería checkear el arboltrivial1 no sea diferente de 2" $
                evaluar arbolTrivial1 `shouldNotBe` 3

            it "Debería checkear que evaluar a arboltrivial2 sea 0" $
                evaluar arbolTrivial2 `shouldBe` 0

            it "Debería checkear el arboltrivial2 que no espera otro numero que no sea 2" $
                evaluar arbolTrivial2 `shouldNotBe` 1

            it "Debería checkear el arboltrivial2 que no espera otro numero que no sea 2" $
                evaluar arbolGrande2 `shouldNotBe` 4
