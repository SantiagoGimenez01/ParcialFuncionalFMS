module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

type Palabra = String
type Verso = String
type Estrofa = [Verso]
type Artista = String -- Solamente interesa el nombre

esVocal :: Char -> Bool
esVocal = flip elem "aeiou"

tieneTilde :: Char -> Bool
tieneTilde = flip elem "áéíóú"

cumplen :: (a -> b) -> (b -> b -> Bool) -> a -> a -> Bool
cumplen f comp v1 v2 = comp (f v1) (f v2)

--Punto 1

coincideUltimaVocal :: Palabra -> Palabra -> Bool
coincideUltimaVocal p1 p2 = ultimaVocal p1 == ultimaVocal p2

coincideAnteultimaVocal :: Palabra -> Palabra -> Bool
coincideAnteultimaVocal p1 p2 = anteultimaVocal p1 == anteultimaVocal p2

filtrarVocales :: Palabra -> [Char]
filtrarVocales = filter (\letra -> esVocal letra || tieneTilde letra)

ultimaVocal :: Palabra -> Char
ultimaVocal = last . filtrarVocales

anteultimaVocal :: Palabra -> Char
anteultimaVocal palabra = (reverse . filtrarVocales) palabra !! 1

ultimasTresLetras :: Palabra -> [Char]
ultimasTresLetras = reverse . take 3 . reverse

coincidenUltimasTresLetras :: Palabra -> Palabra -> Bool
coincidenUltimasTresLetras p1 p2 = ultimasTresLetras p1 == ultimasTresLetras p2

esRimaAsonante :: Palabra -> Palabra -> Bool
esRimaAsonante p1 p2 = 
    cumplen filtrarVocales coincideUltimaVocal p1 p2 && cumplen filtrarVocales coincideAnteultimaVocal p1 p2
--coincideUltimaVocal p1 p2 && coincideAnteultimaVocal p1 p2 && (p1 /= p2)

esRimaConsonante :: Palabra -> Palabra -> Bool
esRimaConsonante = cumplen ultimasTresLetras coincidenUltimasTresLetras 

{-
Las clases de equivalencia son
  - Dos palabras riman por rima asonante
  - Dos palabras riman por rima consonante
  - Dos palabras iguales no riman
  - Dos palabras sin conexion no riman
-}
