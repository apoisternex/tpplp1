module Documento
  ( Doc,
    vacio,
    linea,
    texto,
    foldDoc,
    (<+>),
    indentar,
    mostrar,
    imprimir,
  )
where
import Control.Arrow (ArrowZero(zeroArrow))
import Text.Read.Lex (Lexeme(String))
import Debug.Trace (trace)

data Doc
  = Vacio
  | Texto String Doc
  | Linea Int Doc
  deriving (Eq, Show)

vacio :: Doc
vacio = Vacio

linea :: Doc
linea = Linea 0 Vacio

texto :: String -> Doc
texto t | '\n' `elem` t = error "El texto no debe contener saltos de línea"
texto [] = Vacio
texto t = Texto t Vacio

foldDoc :: ( String -> a -> a ) -> ( Int -> a -> a ) -> a -> Doc -> a
foldDoc fTexto fLinea z d = case d of
    Texto s ds -> fTexto s (foldDoc fTexto fLinea z ds)
    Linea n ds -> fLinea n (foldDoc fTexto fLinea z ds)
    Vacio -> z

-- NOTA: Se declara `infixr 6 <+>` para que `d1 <+> d2 <+> d3` sea equivalente a `d1 <+> (d2 <+> d3)`
-- También permite que expresiones como `texto "a" <+> linea <+> texto "c"` sean válidas sin la necesidad de usar paréntesis.
infixr 6 <+>

recrDoc :: ( String -> a -> Doc -> a ) -> ( Int -> a -> Doc -> a ) -> a -> Doc -> a
recrDoc fTexto fLinea z d = case d of
    Texto s ds -> fTexto s (recrDoc fTexto fLinea z ds) ds
    Linea n ds -> fLinea n (recrDoc fTexto fLinea z ds) ds
    Vacio -> z

(<+>) :: Doc -> Doc -> Doc
(<+>) d1 d2 = recrDoc (\s1 rec ds1-> 
                          case (ds1, d2) of
                            (Vacio, Texto s2 ds2) -> (Texto (s1 ++ s2) ds2)
                            _ -> (Texto s1 rec)
                       ) (\n rec ds -> Linea n rec) d2 d1

-- concatDoc :: Doc -> Doc -> Doc 
-- concatDoc d1 d2 = foldDoc (Texto) Linea d2 d1

indentar :: Int -> Doc -> Doc
indentar i (Linea n ds) = Linea n (foldDoc Texto (\ n rec -> Linea (n + i) rec) Vacio ds)
indentar i d = foldDoc Texto (\ n rec -> Linea (n + i) rec) Vacio d

mostrar :: Doc -> String
mostrar = error "PENDIENTE: Ejercicio 4"

-- | Función dada que imprime un documento en pantalla

-- ghci> imprimir (Texto "abc" (Linea 2 (Texto "def" Vacio)))
-- abc
--   def

imprimir :: Doc -> IO ()
imprimir d = putStrLn (mostrar d)
