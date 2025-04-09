module PPON where

import Documento
import GHC.Exts.Heap.Utils (dataConNames)

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
intercalar doc = foldr1 (\ x acc -> x <+> doc <+> acc)

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
pponADoc p = case p of
    TextoPP st  -> texto (show st)
    IntPP i     -> texto (show i)
    ObjetoPP q  -> if pponObjetoSimple p
      then aplanar ( entreLlaves (foldr (\ (s, pom) recu -> (texto (show s ++ ": ") <+> pponADoc pom) : recu) [] q) )
      else entreLlaves ( foldr (\ (s, pom) recu -> (texto (show s ++ ": ") <+> pponADoc pom) : recu) [] q )
