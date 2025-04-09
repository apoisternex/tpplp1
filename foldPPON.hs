data PPON
  = TextoPP String
  | IntPP Int
  | ObjetoPP [(String, PPON)]
  deriving (Eq, Show)

foldPPON :: a -> (String -> a) -> (Int -> a) -> ((String, PPON) -> a -> a) -> PPON -> a
foldPPON base ftexto fint fobjeto popo = case popo of
    TextoPP  s      -> ftexto  s 
    IntPP    i      -> fint    i 
    ObjetoPP lista  -> foldr (\(st, pon) acc -> fobjeto (st, pon) (foldPPON base ftexto fint fobjeto pon)) base lista
