module PPON where
import Documento

data PPON
  = TextoPP String
  | IntPP Int
  | ObjetoPP [(String, PPON)]
  deriving (Eq, Show)


pponAtomico :: PPON -> Bool
pponAtomico p = case p of
    ObjetoPP  _  -> False
    _            -> True

pponObjetoSimple :: PPON -> Bool
pponObjetoSimple (ObjetoPP lista) = foldr (\x acc -> (pponAtomico . snd) x && acc) True lista

intercalar :: Doc -> [Doc] -> Doc
intercalar doc []     = vacio
intercalar doc lista = foldr1 (\ x acc -> x <+> doc <+> acc) lista

entreLlaves :: [Doc] -> Doc
entreLlaves [] = texto "{ }"
entreLlaves ds =
  texto "{"
    <+> indentar
      2
      ( linea
          <+> intercalar (texto "," <+> linea) ds
      )
    <+> linea
    <+> texto "}"

aplanar :: Doc -> Doc
aplanar = foldDoc (\ tex recu -> texto tex <+> recu) (\ _ recu -> texto " " <+> recu) vacio

pponADoc :: PPON -> Doc
{-En esta funcion, estamos definiendo un ejemplo explicito de recursion estructural-}
pponADoc p = case p of
  {-Esto se debe a que la funcion cumple con las 2 características que necesitamos para mostrar que, en efecto,
  se usa la recursion estructural-}
    TextoPP st  -> texto (show st) {-En principio necesitamos que los casos base devuelvan valores fijos o, en este caso,
    funciones sobre los ultimos elementos(ya que PPON no tiene constructores a los cuales no les pasemos ningun elemento)
    que no tengan llamados recursivos-}
    IntPP i     -> texto (show i) {-lo mismo se cumple para este caso base-}
    ObjetoPP q  -> if pponObjetoSimple p {-Luego, argumentamos que este llamado recursivo es caracteristico de la recursion
      estructural.
      En principio veamos que ambos casos aplican funciones que estan compuestas o bien por aplanar y entreLlaves 
      o solo por esta última. Ambas son funciones estructurales representadas como instancias de foldr.
      Luego, ambos casos recursivos llaman a la misma funcion por dentro la cual esta definida como una instancia de foldr,
      pero que llama a pponADoc por dentro y por lo tanto, vuelve a esta una recursion explicita.
      Sin embargo, lo que nos importa ver es que en ningun momento se accede a la variable pom suelta, sino que se hace el
      llamado recursivo sobre esta misma(pponADoc pom). Como estamos accediendo directamente a la primer tupla de la lista, y el valor que
      devuelve la función lo estamos concatenando al llamado recursivo de xs (representado por recu) sin quitarla de la aplicacion
      de la funcion, y ademas el caso base devuelve el valor fijo [], podemos decir que esta recursion es estructural 
      encapsulada en multiples funciones de recursion estructural-}
      then aplanar ( entreLlaves (foldr (\ (s, pom) recu -> (texto (show s ++ ": ") <+> pponADoc pom) : recu) [] q) )
      else entreLlaves ( foldr (\ (s, pom) recu -> (texto (show s ++ ": ") <+> pponADoc pom) : recu) [] q )
