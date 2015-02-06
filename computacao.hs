module Computacao
( split
, fibonacci
, fatorial
, collatz
, prodsc
, prodvec
, zeta
, proj
, Grafo
, Polinomio
, Automato
, MaquinaDeTuring
, Regex
, mtrxSoma
)where
split:: Eq t => t -> [t] -> [[t]]
split tok [] = return  []
split tok texto = r:(split tok novo)
                  where
                       r = takeWhile (/=tok) texto
                       novo = drop ((length r)+1) texto
fatorial::Integer->Integer
fatorial n
         |n==0 =1
         |n>0  =product [1..n]
fibonacci::Integer->Integer
fibonacci n
          |or [n==1,n==2] =1
          |n>1 =fibonacci (n-1) + fibonacci (n-2)
collatz ::Integral t => t -> [t]
collatz 1=[1]
collatz n
        | mod n 2 ==0 =n:collatz(div n 2)
        | otherwise =n:collatz(3*n+1)
prodsc::Num a => [a]-> [a]->a
prodsc u []=0
prodsc [] v=0
prodsc u v= (head u * head v) + prodsc (tail u)  (tail v)
prodvec::Num t => [t]->[t] ->[t]
prodvec [a,b,c] [d,e,f]=[b*f-c*e,c*d-a*f,a*e-b*d]
zeta::(Ord a, Floating a, Enum a) => a->a->a
zeta p k
        |k>1 = sum [1/n**p|n<-[1..k]]
proj::Fractional t => [t] -> [t] -> t
proj u v= (prodsc u v)/sum [n^2|n<-u]
data Automato = Atomato{ alfabeto::String
                       , estados::String
                       , transicao::[(Char,Char,Char)]
                       , finais::String
                       , iniciais::String
                       }deriving(Show)
type Regex = String
type Polinomio=[Float]
type Grafo = [[(Int,Float)]]
data MaquinaDeTuring = MaquinaDeTuring{ inicial::Char
                                      , alfabetoTranscricao::String
                                      , transicaoM::[(Char,Char,Char,Char,Char)]
                                      , estadosM::String
                                      , alfabetoFita::String
                                      , finaisM::String
                                      , branco::Char
                                      , inicioDeFita::Char
                                      }deriving(Show)
mtrxSoma::Num b=>[[b]]->[[b]]->[[b]]
mtrxSoma a b = map (\(u,v)->soma u v) (zip a b) where
                          soma u v= map (\(x,y)->x+y) (zip u v)
