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