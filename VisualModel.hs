module VisualModel where


{- Sobre el tablero el usuario puede colocar diferentes figuras
   que representan predicados de una signatura. Una configuración
   de tablero luego se traduce a un modelo de la signatura.-}
   
-- un board válido tendrá todos ElemBoard diferentes (es decir con su campo "elem" distinto)
data Board = Board {
                board :: [ElemBoard]
              , size :: Int
              -- para cada relación de la signatura definimos un criterio para decidir si n elementos relacionados.
              -- La función asociada a cada relación define la interpretación en el modelo visual.
              , interpRels :: M.Map Relation ([ElemBoard] -> Bool)
}


data Coord = Coord {
                xcoord :: Int
              , ycoord :: Int
}


triangulo = Predicate "Tr"
cuadrado = Predicate "Cuad"
circulo = Predicate "Circ"


s :: Signature
s = Signature {
        predicates = [triangulo,circulo,cuadrado]
}

data ElemBoard univ = ElemBoard {
                    elem :: univ
                  , predicates :: M.Map Predicate Bool -- Si tenemos (p,True) significa que el elemento cumple el predicado.
                  , coord :: Coord
--                   , efigure :: Figure
--                   , esize :: Maybe FigureSize
--                   , ecolor :: Maybe FigureColor
}

b = Board { algo }

elems = map snd (board b)

-- Ahora para generar las relaciones n-arias, estamos muy atados a la signatura
-- en particular.

crearRelDer :: Board -> Relation -> [[univ]]
crearRelDer b r =
    map (\(c,e) -> foldl (\l (c',e') -> if (maybe (error "El Board no se corresponde con la signatura")
                                                  (\f -> f [e,e']) (M.lookup r (interpRels b)))
                                            then [c,c']:l
                                            else l) [] (board b)) (board b)
                                            


crearRelaciones :: Signature -> Board -> M.Map Relation [[univ]]
crearRelaciones s b =
    foldl (\m r -> M.insert r ) M.empty (relations s)
                                            
                                            
-- Genero todas los predicados de la signatura:
getrel p = map elem $ catMaybes $ map (M.lookup p predicates) elems

crearPredicados :: Signature -> [ElemBoard] -> M.Map Predicate [univ]
crearPredicados s elems = 
    foldl (\m p -> M.insert p (getrel p)) M.empty (predicates s)

                  
boardToModel :: Signature -> Board -> Model

                  