-- Módulos --
module Sat where
import Data.List (intersect, union, sort, nub)

-- Pre - código --

data Prop = Var String | Cons Bool | Not Prop
 | And Prop Prop | Or Prop Prop
 | Impl Prop Prop | Syss Prop Prop deriving (Eq)


instance Show Prop where 
                    show (Cons True) = "Verdadero"
                    show (Cons False) = "Falso"
                    show (Var p) = p
                    show (Not p) = "¬" ++ show p
                    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
                    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
                    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
                    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

negar :: Prop -> Prop
negar (Var p) = Not (Var p)
negar (Cons True) = (Cons False)
negar (Cons False) = (Cons True)
negar (Not f) = f
negar (And f1 f2) = (Or (negar f1) (negar f2))
negar (Or f1 f2) = (And (negar f1) (negar f2))
negar (Impl f1 f2) = (And f1 (negar f2))
negar (Syss f1 f2)= negar (And (Impl f1 f2) (Impl f2 f1)) 


distribuir :: Prop -> Prop
distribuir (And p q) = And (distribuir p) (distribuir q)
distribuir (Or (And p1 p2) q) = And (distribuir (Or p1 q)) (distribuir (Or p2 q))
distribuir (Or p (And q1 q2)) = And (distribuir (Or p q1)) (distribuir (Or p q2))
distribuir (Or p q) = Or (distribuir p) (distribuir q)
distribuir p = p

--- E1.1 Implementar la funcion fnn que convierte una fórmula proproposicional en su forma normal negativa.

fnn :: Prop -> Prop
fnn (Var p) = (Var p)
fnn (Not p) = negar p  
fnn (And p q) = And (fnn p) (fnn q)  
fnn (Or p q) = Or (fnn p) (fnn q)  
fnn (Impl p q) = fnn (Or (Not p) q) 
fnn (Syss p q) = fnn (And (Impl p q) (Impl q p))  
  
-- E1.2 Impementar la función fnc, que convierte una fórmula proposicional en su forma normal conjuntiva. Se recomienda usar la función fnn.

fnc :: Prop -> Prop
fnc p = distribuir (fnn p)
  
-- E2.1 Crear un sinónimo Literal, que será igual a Prop por simplicidad, aunque solo deberían ser variables o negaciones de variables. 

type Literal = Prop

-- E2.2 Crear un sinónimo Clausula, que representará las claúsuas como conjunto de literales.

type Clausula = [Literal]

-- E2.3 Definir la función clausulas que dada una fórmula en FNC, devuelve una lista con cláusulas que la forman.

clausulas :: Prop -> [Clausula]
clausulas (And p q) = clausulas p ++ clausulas q  
clausulas (Or p q)  = [[p, q]]  
clausulas p         = [[p]]

-- E2.4 Definir la función resolución que dadas dos cláusulas, devuelve el resolvente obtenido después de aplicar la regla de resolución binaria. Se puede asumir que se puede obtener un resolvente a partir de los argumentos.

resolucion :: Clausula -> Clausula -> Clausula
resolucion c1 c2 = [l | l <- (c1 ++ c2), not (negacion l `elem` c1 || negacion l `elem` c2)]
  where
    negacion (Not p) = p
    negacion p = Not p


-- E3.1 Definir la función hayResolvente, que determina si es posible obtener un resolvente a partir de dos cláusulas.

hayResolvente :: Clausula -> Clausula -> Bool
hayResolvente c1 c2 = hayResolventeAux c1
  where
    hayResolventeAux [] = False
    hayResolventeAux (l:ls) = (negacion l `elem` c2) || hayResolventeAux ls
    negacion (Not p) = p
    negacion p = Not p

-- E3.2 Definir la función saturacion, que dada una fórmula proposicional, determina si esta es satisfacible o no usando el algoritmo de saturación.

saturacion :: [Clausula] -> Bool
saturacion clausulas
  | [] `elem` clausulas = False  -- Si hay una cláusula vacía, no es satisfacible
  | otherwise = or [resolucion c1 c2 == [] | c1 <- clausulas, c2 <- clausulas, c1 /= c2]

