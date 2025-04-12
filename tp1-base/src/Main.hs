module Main (main) where

import Documento
import PPON
import Test.HUnit

main :: IO ()
main = runTestTTAndExit allTests

allTests :: Test
allTests =
  test
    [ "Ejercicio 2" ~: testsEj2,
      "Ejercicio 3" ~: testsEj3,
      "Ejercicio 4" ~: testsEj4,
      "Ejercicio 6" ~: testsEj6,
      "Ejercicio 7" ~: testsEj7,
      "Ejercicio 8" ~: testsEj8,
      "Ejercicio 9" ~: testsEj9
    ]

testsEj2 :: Test
testsEj2 =
  test
    [ vacio <+> vacio ~?= vacio,
      texto "a" <+> texto "b" ~?= texto "ab",
      (texto "a" <+> linea) <+> texto "b" ~?= texto "a" <+> linea <+> texto "b",
      --Tests hechos por nosotros
      (texto "a" <+> texto "b") <+> linea ~?= texto "a" <+> texto "b" <+> linea,
      (texto "a" <+> texto "b") <+> texto "c" ~?= texto "a" <+> texto "b" <+> texto "c",
      texto "a" <+> texto "b" ~?= (<+>) (texto "a") (texto "b"),
      (linea <+> texto "a") <+> texto "b" ~?= linea <+> texto "ab", --concatenar primero una linea
      vacio <+> texto "a" ~?= texto "a", --concatenar a vacio
      texto "a" <+> vacio ~?= texto "a", --concatenar un vacio a otro no vacio
      vacio <+> linea ~?= linea, --concatenar a vacio pero con linea
      linea <+> vacio ~?= linea   -- al revés
    ]

testsEj3 :: Test
testsEj3 =
  test
    [ indentar 2 vacio ~?= vacio,
      indentar 2 (texto "a") ~?= texto "a",
      indentar 2 (texto "a" <+> linea <+> texto "b") ~?= texto "a" <+> indentar 2 (linea <+> texto "b"),
      indentar 2 (linea <+> texto "a") ~?= indentar 1 (indentar 1 (linea <+> texto "a")),
            --Tests hechos por nosotros
      indentar 2 (linea <+> linea <+> linea) ~?= indentar 2 linea <+> indentar 2 linea <+> indentar 2 linea, --indentar distributiva
      indentar 2 (texto "a" <+> indentar 1 linea) ~?= indentar 3 (texto "a" <+> linea), --indentar suma de indentados.
      indentar 0 (linea <+> texto "a") ~?= linea <+> texto "a", --indentar 0 veces
      indentar 2 (linea <+> indentar 4 linea) ~?= indentar 2 linea <+> indentar 6 linea --distributiva con casos distinto de 0
    ]

testsEj4 :: Test
testsEj4 =
  test
    [ mostrar vacio ~?= "",
      mostrar linea ~?= "\n",
      mostrar (indentar 2 (texto "a" <+> linea <+> texto "b")) ~?= "a\n  b",
      -- Tests hechos por nosotros
      mostrar (indentar 2 (linea <+> texto "a")) ~?= "\n  a"
    ]

pericles, merlina, addams, familias :: PPON
pericles = ObjetoPP [("nombre", TextoPP "Pericles"), ("edad", IntPP 30)]
merlina = ObjetoPP [("nombre", TextoPP "Merlina"), ("edad", IntPP 24)]
addams = ObjetoPP [("0", pericles), ("1", merlina)]
familias = ObjetoPP [("Addams", addams)]

testsEj6 :: Test
testsEj6 =
  test
    [ pponObjetoSimple pericles ~?= True,
      pponObjetoSimple addams ~?= False,
      -- Tests hechos por nosotros (aunque usan los objetos definidos por la catedra, pero con estos alcanza para ver los casos que faltan)
      pponObjetoSimple familias ~?= False -- ¿toma como objeto complejo a algo que tiene adentro un objeto complejo?
    ]

a, b, c :: Doc
a = texto "a"
b = texto "b"
c = texto "c"

testsEj7 :: Test
testsEj7 =
  test
    [ mostrar (intercalar (texto ", ") []) ~?= "",
      mostrar (intercalar (texto ", ") [a, b, c]) ~?= "a, b, c",
      mostrar (entreLlaves []) ~?= "{ }",
      mostrar (entreLlaves [a, b, c]) ~?= "{\n  a,\n  b,\n  c\n}",
      --Tests hechos por nosotros
      mostrar (entreLlaves [a, linea, b, c]) ~?= "{\n  a,\n  \n  ,\n  b,\n  c\n}"
    ]

testsEj8 :: Test
testsEj8 =
  test
    [ mostrar (aplanar (a <+> linea <+> b <+> linea <+> c)) ~?= "a b c",
      -- Tests hechos por nosotros
      mostrar (aplanar vacio) ~?= "",
      mostrar (aplanar (indentar 5 (linea <+> linea))) ~?= "  ",
      mostrar (aplanar (texto "a" <+> texto "b")) ~?= "ab",
      mostrar (aplanar (indentar 2 (a <+> linea <+> b <+> linea <+> c))) ~?= "a b c"
    ]
objetoMixto :: PPON
objetoMixto = ObjetoPP [("familias",familias),("saludo",TextoPP "hola"),("clon de Pericles",pericles)]
testsEj9 :: Test
testsEj9 =
  test
    [ mostrar (pponADoc pericles) ~?= "{ \"nombre\": \"Pericles\", \"edad\": 30 }",
      mostrar (pponADoc addams) ~?= "{\n  \"0\": { \"nombre\": \"Pericles\", \"edad\": 30 },\n  \"1\": { \"nombre\": \"Merlina\", \"edad\": 24 }\n}",
      mostrar (pponADoc familias) ~?= "{\n  \"Addams\": {\n    \"0\": { \"nombre\": \"Pericles\", \"edad\": 30 },\n    \"1\": { \"nombre\": \"Merlina\", \"edad\": 24 }\n  }\n}",
      --Tests hechos por nosotros
      mostrar (pponADoc (TextoPP "hola")) ~?= "\"hola\"",
      mostrar (pponADoc (IntPP 6)) ~?= "6",
      mostrar (pponADoc objetoMixto) ~?= "{\n  \"familias\": {\n    \"Addams\": {\n      \"0\": { \"nombre\": \"Pericles\", \"edad\": 30 },\n      \"1\": { \"nombre\": \"Merlina\", \"edad\": 24 }\n    }\n  },\n  \"saludo\": \"hola\",\n  \"clon de Pericles\": { \"nombre\": \"Pericles\", \"edad\": 30 }\n}"
    ]
