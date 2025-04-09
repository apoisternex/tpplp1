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
aplanar dc = texto (foldDoc (\ s recu -> s ++ " " ++ recu) (\ _ recu -> recu) "" dc)

pponADoc :: PPON -> Doc
pponADoc = error "PENDIENTE: Ejercicio 9"
