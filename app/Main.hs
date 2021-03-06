import Diccionario
import Data.Maybe
import Arbol23
import Tesoro
import Test.HUnit


--Ejecución de los tests
main :: IO Counts
main = do 
    putStrLn "NOTA: Preferimos usar HSpec. Por favor, correr \"stack test\""
    runTestTT allTests

allTests = test [
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7,
  "ejercicio8" ~: testsEj8,
  "ejercicio9" ~: testsEj9,
  "ejercicio10" ~: testsEj10
  ]

testsEj2 = test [
  [0,1,2,3,4,5,6,7] ~=? internos arbolito1,
  "abcdefghi" ~=? hojas arbolito1,
  [True,False,True] ~=? internos arbolito2,
  [1,2,3,2,3,4,3,4,5,4] ~=? take 10 (hojas arbolito3)
  ]

testsEj3 = test [
  [0,1,-1,5] ~=? hojas (incrementarHojas arbolito2)
  ]

testsEj4 = test [
  [1,2,3,2,3,4,3,4,5,4,5,6,0,0,0,0,0] ~=? hojas (truncar 0 6 arbolito3)
  ]

testsEj5 = test [
  22 ~=? evaluar (truncar 0 6 arbolito3)
  ]

testsEj6 = test [
  0 ~=? 0 --Cambiar esto por tests verdaderos.
  ]

testsEj7 = test [
  0 ~=? 0 --Cambiar esto por tests verdaderos.
  ]

testsEj8 = test [
  0 ~=? 0 --Cambiar esto por tests verdaderos.
  ]
  
testsEj9 = test [
  0 ~=? 0 --Cambiar esto por tests verdaderos.
  ]
  
testsEj10 = test [
  Just "alfajor" ~=? busquedaDelTesoro "inicio" ((=='a').head) dicc2
  ]