import GHC.Base (VecElem(Int16ElemRep))
--Renombre de tipos:
type Punto2D = (Float, Float)
type Anio = (Int) 
type Bisiesto = (Bool)
type Punto3D = (Float, Float, Float)


estanRelacionados :: (Int, Int) -> Bool
estanRelacionados (a, b)  
  | a == 0 || b == 0 = False
  | a * a + a * b * k == 0 = True
  | otherwise = False
   where k =  ((-a) * a)`div`(a*b)

productoInterno :: Punto2D -> Punto2D -> Punto2D 
productoInterno (a, b) (c, d) = (fst(a, b) + fst (c, d), snd(a, b) + snd(c, d))
  
esParMenor :: (Int, Int) -> (Int, Int) -> (Int, Int)
esParMenor (a, b) (c, d)
  | fst(a, b) > fst(c, d) && snd(a, b) > snd(c, d) = (fst(a, b), fst(c, d))
  | fst(a, b) < fst(c, d) && snd(a, b) < snd(c, d) = (snd(a, b), snd(c, d))
  | fst(a, b) > fst(c, d) && snd(a, b) < snd(c, d) = (fst(a, b), snd(c, d))
  | otherwise = (snd(a, b), fst(c, d))

distancia :: (Int, Int) -> (Int, Int) -> Float
distancia (a, b) (c, d) = sqrt(fromIntegral ((b - a)^2 + (d - c)^2))

sumarTerna :: (Int, Int, Int) -> Int
sumarTerna (a, b, c) = a + b + c

sumarMultiplos :: (Int, Int, Int) -> Int -> Int
sumarMultiplos (a, b, c) n
  | a `mod` n == 0 && b `mod` n == 0 && c `mod` n == 0 = a + b + c
  | a `mod` n == 0 && b `mod` n == 0                   = a + b
  | a `mod` n == 0 && c `mod` n == 0                   = a + c
  | b `mod` n == 0 && c `mod` n == 0                   = b + c
  | a `mod` n == 0                                     = a
  | b `mod` n == 0                                     = b
  | c `mod` n == 0                                     = c
  | otherwise                                          = 0

posPrimerPar :: (Int, Int, Int) -> Int
posPrimerPar (a, b, c)
  | a `mod` 2 == 0 = a
  | b `mod` 2 == 0 = b
  | c `mod` 2 == 0 = c
  | otherwise = 4

crearPar :: tx1 -> tx2 -> (tx1, tx2)
crearPar tx1 tx2 = (tx1, tx2)

invertirPar :: (tx1, tx2) -> (tx2, tx1)
invertirPar (tx1, tx2) = (tx2, tx1)


f :: Int -> Int
f n
  | n <= 7 = n^2
  | n > 7 = 2*n - 1

g :: Int -> Int
g n 
  | n `mod` 2 == 0 = n `div` 2
  | otherwise = 3*n + 1


todosMenores :: (Int, Int, Int) -> Bool
todosMenores (a, b, c)
  | f a > g a && f b > g b && f c > g c = True
  | otherwise = False 

añoBisiesto :: (Anio) -> (Bisiesto)
añoBisiesto n 
  | n `mod` 4 /= 0 || (n `mod` 100 == 0 && n `mod` 400 /= 0) = False
  |otherwise = True

distanciaManhattan :: (Punto3D) -> (Punto3D) -> Float
distanciaManhattan (x1, x2, x3) (y1, y2, y3) = sqrt((y1 - x1)^2 + (y2 - x2)^2 + (y3 - x3)^2)

valorAbsoluto :: Int -> Int
valorAbsoluto n 
  | n >=0 = n
  | n < 0 = n * (-1)

ultimosDosDigitos :: Int -> Int
ultimosDosDigitos n = valorAbsoluto(n) `mod` 10 + (valorAbsoluto(n) `div` 10) `mod` 10

comparar :: Int -> Int -> Int
comparar k n 
  | ultimosDosDigitos(k) < ultimosDosDigitos(n) = 1
  | ultimosDosDigitos(k) > ultimosDosDigitos(n) = -1
  | ultimosDosDigitos(k) == ultimosDosDigitos(n) = 0

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial(n-1)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

parteEntera :: Float -> Int
parteEntera n
  | n < 0.5 = 0
  | n >= 0.5 && n <= 1 = 1
  | otherwise = 1 + parteEntera(n-1)

esDivisible :: Int -> Int -> Bool
esDivisible a b 
  | a == 0 = True
  | a < b = False
  | otherwise = esDivisible (a-b) b

sumaImpares :: Int -> Int
sumaImpares n
  | n == 0 = 0 
  | n == 1 = 1
  | n `mod` 2 == 0 = sumaImpares(n-1)
  | otherwise = n + sumaImpares(n-1) 

sumaImparesDos :: Int -> Int 
sumaImparesDos n
  | n == 0 = 0  
  | n == 1 = 1
  | otherwise = esImpar n + sumaImparesDos(n-1)

esImpar :: Int -> Int
esImpar n 
  | n `mod` 2 == 0 = 0
  | otherwise = n

sumatoriaImpares :: Int -> Int 
sumatoriaImpares 0 = 0
sumatoriaImpares n = (2*n - 1) + sumatoriaImpares(n-1)

medioFact :: Int -> Int 
medioFact 0 = 1
medioFact n = n * medioFact(n-2)

digitosIguales :: Int -> Bool
digitosIguales n
  | n < 10 = True
  | n `mod` 10 /= (n `div` 10) `mod` 10 = False
  | otherwise = digitosIguales(n `div` 10)


longitudNumero :: Int -> Int 
longitudNumero n
  | n < 10 = 1
  | otherwise = 1 + longitudNumero(n`div`10)


iesimoDigito :: Int -> Int -> Int
iesimoDigito n i 
  | longitudNumero(n) < 2 = n 
  | otherwise = iesimoDigito(n`div`(10^(i-1)) `mod` 10) i 

sumatoriaTodos :: Int -> Int
sumatoriaTodos 1 = 1
sumatoriaTodos n = n `mod` 10 + sumatoriaTodos(n `div` 10) `mod` 10

esCapicua :: Int -> Bool
esCapicua n
  | n < 10 = True
  | n `div` 10^(longitudNumero(n) - 1) /=  n `div` 10 = False
  | longitudNumero n == 2 = True
  | otherwise = esCapicua((n `div` 10) `mod` 10^(longitudNumero(n) - 2)) 

sumatoriaUno :: Int -> Int
sumatoriaUno 0 = 1
sumatoriaUno n = 2^n + sumatoriaUno(n-1)

sumatoriaDos :: Int -> Int -> Int
sumatoriaDos 0 0= 0 
sumatoriaDos n q = n^q + sumatoriaDos(n-1) (q-1)

sumatoriaTres :: Int -> Int -> Int
sumatoriaTres n q
  | n <= 0    = 0
  | otherwise = q^(2 * n) + sumatoriaTres (n - 1) q


aproximacion :: Float -> Int 
aproximacion n 
  | n < 0.5 = 0
  | n < 1 = 1
  | otherwise = 1 + aproximacion (n-1)

aproxEuler :: Int -> Float
aproxEuler n 
  | n == 0 = 1
  | otherwise = (1.0 / fromIntegral (factorial n)) + aproxEuler (n-1)


raizDeDos :: Int -> Float
raizDeDos 1 = 1.0
raizDeDos n = 1.0 +  1.0 / (2 + raizDeDos(n-1))

dobleSumatoria :: Int -> Int -> Int
dobleSumatoria i j 
  | i == 0 = 0
  | otherwise = sumatoriaAux i j + dobleSumatoria (i-1) j
  where 
    sumatoriaAux :: Int -> Int -> Int 
    sumatoriaAux _ 0 = 0
    sumatoriaAux i j = i^j + sumatoriaAux i (j-1)

sumaPotencias :: Int -> Int -> Int -> Int
sumaPotencias q i j
  | i == 0 || j == 0 = 0
  | i == j = sumaPotenciasAux q i j 
  | otherwise = q ^ (i + j) + sumaPotencias q (i - 1) (j - 1)
  where 
    sumaPotenciasAux :: Int -> Int -> Int -> Int
    sumaPotenciasAux _ _ (-1) = 0
    sumaPotenciasAux q i j = q ^(i + j) + sumaPotenciasAux q i (j - 1)

sumaRacionales :: Int -> Int -> Float
sumaRacionales 0 _ = 0  
sumaRacionales n m = sumaAux n m + sumaRacionales (n - 1) m
 where 
  sumaAux :: Int -> Int -> Float
  sumaAux _ 0 = 0
  sumaAux n m = (fromIntegral n / fromIntegral m) + sumaAux n (m - 1)

menorDivisor :: Int -> Int 
menorDivisor n = menorDivisorAux n 2 
  where 
    menorDivisorAux :: Int -> Int -> Int
    menorDivisorAux n k
      | n `mod` k == 0 = k
      | otherwise      = menorDivisorAux n (k + 1)

esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo n = esPrimoAux n 2

esPrimoAux :: Int -> Int -> Bool
esPrimoAux n k 
  | mod n k /= 0 = esPrimoAux n (k + 1)
  | n == k = True
  | otherwise = False

esPrimoDos :: Int -> Int 
esPrimoDos 1 = 0
esPrimoDos n = esPrimoAux n 2
   where 
    esPrimoAux :: Int -> Int -> Int
    esPrimoAux n k 
      | mod n k /= 0 = esPrimoAux n (k + 1)
      | n == k = n
      | otherwise = 0

noEsPrimo :: Int -> Int 
noEsPrimo 1 = 0
noEsPrimo n = noEsPrimoAux n 2
   where 
    noEsPrimoAux :: Int -> Int -> Int
    noEsPrimoAux n k 
      | mod n k == 0 = 1
      | mod n k /= 0 = noEsPrimoAux(k + 1) 
      | otherwise = 0 

sumatoriaPrimo :: Int -> Int 
sumatoriaPrimo 0 = 0
sumatoriaPrimo n = esPrimoDos n + sumatoriaPrimo(n - 1)


{-problema hayQueCodificar (c: Char, mapeo: seq⟨Char x Char⟩ ) : Bool {
  requiere: {No hay elementos repetidos entre las primeras componentes de mapeo}
  requiere: {No hay elementos repetidos entre las segundas componentes de mapeo}
  asegura: {res = true <=> c es igual a la primera componente de alguna tupla de mapeo}-}


hayQueCodificar :: Char -> [(Char, Char)] -> Bool
hayQueCodificar _ [] = False
hayQueCodificar i (x:xs)
  | i == fst x = True 
  | otherwise = hayQueCodificar i xs 

{-problema cuantasVecesHayQueCodificar (c: Char, frase: seq⟨Char⟩, mapeo: seq⟨Char x Char⟩ ) : Z {
  requiere: {No hay elementos repetidos entre las primeras componentes de mapeo}
  requiere: {No hay elementos repetidos entre las segundas componentes de mapeo}
  requiere: {|frase| > 0 }
  requiere: {c pertenece a frase}
  asegura: {(res = 0 y hayQueCodificar (c, mapeo) = false) o (res = cantidad de veces que c aparece en frase y hayQueCodificar (c, mapeo) = true)}-}

cuantasVecesHayQueCodificar :: Char -> [Char] -> [(Char, Char)] -> Int
cuantasVecesHayQueCodificar i xs ys 
  | hayQueCodificar i ys = contadorDeC i xs 
  | otherwise = 0  

contadorDeC :: Char -> [Char] -> Int
contadorDeC _ [] = 0  
contadorDeC i (x:xs)
  | i == x = 1 + contadorDeC i xs
  | otherwise = contadorDeC i xs

  {-problema laQueMasHayQueCodificar (frase: seq⟨Char⟩, mapeo: seq⟨Char x Char⟩ ) : Char {
  requiere: {No hay elementos repetidos entre las primeras componentes de mapeo}
  requiere: {No hay elementos repetidos entre las segundas componentes de mapeo}
  requiere: {|frase| > 0 }
  requiere: {Existe al menos un c que pertenece a frase y hayQueCodificar(c, mapeo)=true}
  asegura: {res = c donde c es el caracter tal que cuantasVecesHayQueCodificar(c, frase, mapeo) es mayor a cualquier otro caracter perteneciente a frase}
  asegura: {Si existen más de un caracter c que cumple la condición anterior, devuelve el que aparece primero en frase }-}

todosDistintosAux :: Char -> [Char] -> Bool
todosDistintosAux _ [] = True
todosDistintosAux i (x:xs)
  | i == x = False
  | otherwise = todosDistintosAux i xs

todosDistintos :: [Char] -> Bool
todosDistintos [_] = True 
todosDistintos (x:y:xs) 
  | todosDistintosAux x (y:xs) = todosDistintos (y:xs) 
  | otherwise = False

laQueMasHayQueCodificar :: [Char] -> [(Char, Char)] -> Char
laQueMasHayQueCodificar [x] _ = x
laQueMasHayQueCodificar (x:y:xs) ys
  | todosDistintos (x:y:xs) = x
  | cuantasVecesHayQueCodificar x (x:y:xs) ys > cuantasVecesHayQueCodificar y (x:y:xs) ys = laQueMasHayQueCodificar (x:xs) ys
  | otherwise = laQueMasHayQueCodificar (y:xs) ys 

  {-problema codificarFrase (frase: seq⟨Char⟩, mapeo: seq⟨Char x Char⟩ ) : seq ⟨Char⟩ {
  requiere: {No hay elementos repetidos entre las primeras componentes de mapeo}
  requiere: {No hay elementos repetidos entre las segundas componentes de mapeo}
  requiere: {|frase| > 0 }
  asegura: {|res| = | frase|}
  asegura: { Para todo 0 <= i < |frase| si hayQueCodificar(frase[i], mapeo) = true entonces res[i]= (mapeo[j])1, para un j tal que 0 <= j < |mapeo| y mapeo[j])0=frase[i]}
  asegura: { Para todo 0 <= i < |frase| si hayQueCodificar(frase[i], mapeo) = false entonces res[i]= frase[i]}-}

invertirTupla :: (Char, Char) -> (Char, Char)
invertirTupla (x,y) = (y,x)

codificarFrase :: [Char] -> [(Char, Char)] -> [Char]
codificarFrase [] _ = []
codificarFrase _ [] = []
codificarFrase (x:xs) (y:ys) 
  | hayQueCodificar x ys = fst (invertirTupla y) : snd (invertirTupla y) : codificarFrase xs ys
  | otherwise = fst y : snd y : codificarFrase xs ys  


--nesimoPrimo :: Int -> Int 
--nesimoPrimo 
--sumatoriaprimos - sumatorianoprimos
