data Doc = Vacio | Texto String Doc | Linea Int Doc deriving Show

foldDoc :: ( String -> a -> a ) -> ( Int -> a -> a ) -> a -> Doc -> a
foldDoc fTexto fLinea z d = case d of
    Texto s ds -> fTexto s (foldDoc fTexto fLinea z ds)
    Linea n ds -> fLinea n (foldDoc fTexto fLinea z ds)
    Vacio -> z

(<+>) :: Doc -> Doc -> Doc 
(<+>) = flip (foldDoc Texto Linea ) 

