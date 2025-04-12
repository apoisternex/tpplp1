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
{-
  Invariante
    Sea Texto s d entonces:
      s no debe ser el string vacío.
      s no debe contener saltos de línea
      d debe ser Vacio o Linea i d’
      
    Sea Linea i d entonces:
      i >= 0
-}
vacio :: Doc
vacio = Vacio

linea :: Doc
linea = Linea 0 Vacio

texto :: String -> Doc
texto t | '\n' `elem` t = error "El texto no debe contener saltos de línea"
texto [] = Vacio
texto t = Texto t Vacio

foldDoc :: (String -> a -> a) -> (Int -> a -> a) -> a -> Doc -> a
foldDoc fTexto fLinea base docu = case docu of
    Vacio -> base
    Texto s ds -> fTexto s (foldDoc fTexto fLinea base ds)
    Linea n ds -> fLinea n (foldDoc fTexto fLinea base ds)

-- NOTA: Se declara `infixr 6 <+>` para que `d1 <+> d2 <+> d3` sea equivalente a `d1 <+> (d2 <+> d3)`
-- También permite que expresiones como `texto "a" <+> linea <+> texto "c"` sean válidas sin la necesidad de usar paréntesis.
infixr 6 <+>

recrDoc :: (String -> a -> Doc -> a) -> (Int -> a -> Doc -> a) -> a -> Doc -> a
recrDoc fTexto fLinea z d = case d of
    Texto s ds -> fTexto s (recrDoc fTexto fLinea z ds) ds 
    Linea n ds -> fLinea n (recrDoc fTexto fLinea z ds) ds 
    Vacio      -> z

{-
    'd1 <+> d1' es igual a 'fTexto s rec ds', 'fLinea n rec ds' o d2. 
    Basta entonces con demostrar que cada uno de estos tres posibles valores cumple con los invariantes.
-}
{- Aclaracion: definimos y utilizamos recrDoc en lugar de FoldDoc porque por la naturaleza del problema,
es mas conveniente usar recursion primitiva ya que queremos chequear las iteraciones posteriores a la cabeza del doc
para ver si estamos viendo la ultima linea o no para asi poder mantener el invariante del Doc al concatenar algo que
termina en texto con algo que comienza en texto.-}
(<+>) :: Doc -> Doc -> Doc
(<+>) d1 d2 = 
    recrDoc 
        (\s1 rec ds1 -> case (ds1, d2) of
            {-
                Texto (s1 ++ s2) ds2 cumple los invariantes porque: 
                (1) s1 y s2 no son vacíos por precondición, por lo tanto, s1 ++ s2 no puede serlo. 
                (2) No añadimos ningún carácter que no estuviese ya en s1 y s2, por lo que no puede haber saltos de línea en s1 ++ s2.
                (3) d2 cumple el invariante por precondición, de modo que ds2 debe ser Vacio o Linea i d’.
            -}
            (Vacio, Texto s2 ds2) -> Texto (s1 ++ s2) ds2 

            {-
                Texto s1 rec cumple los invariantes porque: 
                (1) s1 no es vacío. 
                (2) s1 no contiene saltos de línea 
                (3) rec no es 'Texto _ _' ya que el 'ds' utilizado para producirlo no lo es y fLinea devuelve 'Linea _ _'. 
            -}
            _ -> Texto s1 rec
            -- Linea n rec cumple el invariante porque n >= 0 por precondición. 
        ) 
        (\n rec ds -> Linea n rec) 
        d2 
        d1
{-
  Indentar únicamente modifica a todos los constructores 'Linea n _' aumentándoles n.
  Por precondición n >= 0, por lo que n + i debe serlo también ya que i es positivo por invariante en entrada.
  No puede suceder que indentar devuelva un valor que contenga 'Texto _ (Texto _ _) _', ya que 
  la función nunca convierte 'Vacio' o 'Linea _ _' en 'Texto _ _'.
  Por último, tampoco se modifica el String de cualquier Doc Texto.
-}
indentar :: Int -> Doc -> Doc
indentar i = foldDoc Texto (\ n rec -> Linea (n + i) rec) Vacio 

mostrar :: Doc -> String
mostrar = foldDoc (++) ((.) (++) (\i -> "\n"++ replicate i ' ')) ""

-- | Función dada que imprime un documento en pantalla

-- ghci> imprimir (Texto "abc" (Linea 2 (Texto "def" Vacio)))
-- abc
--   def

imprimir :: Doc -> IO ()
imprimir d = putStrLn (mostrar d)
