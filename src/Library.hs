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
palabrasIguales :: Palabra -> Palabra -> Bool
palabrasIguales p1 p2 = p1 == p2

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

esRima :: Palabra -> Palabra -> Bool 
esRima p1 p2 = (esRimaAsonante p1 p2 || esRimaConsonante p1 p2) && (not . palabrasIguales p1) p2
{-
Las clases de equivalencia son
  - Dos palabras riman por rima asonante
  - Dos palabras riman por rima consonante
  - Dos palabras iguales no riman
  - Dos palabras sin conexion no riman
-}

--Punto 2
ultimaPalabraDeVerso :: Verso -> Palabra
ultimaPalabraDeVerso = last . words

primerPalabraDeVerso :: Verso -> Palabra
primerPalabraDeVerso = head . words

esAnadiplosis :: Verso -> Verso -> Bool
esAnadiplosis v1 v2 = ultimaPalabraDeVerso v1 == primerPalabraDeVerso v2

conjugacionPorMedioDeRimas :: Verso -> Verso -> Bool
conjugacionPorMedioDeRimas = cumplen ultimaPalabraDeVerso esRima

conjugacionHaciendoAnadiplosis :: Verso -> Verso -> Bool
conjugacionHaciendoAnadiplosis = esAnadiplosis

conjugacionDeVersos :: Verso -> Verso -> Bool
conjugacionDeVersos v1 v2 = conjugacionHaciendoAnadiplosis v1 v2 || conjugacionPorMedioDeRimas v1 v2

--Punto 3
type Patron = Estrofa -> Bool
type PatronCombinado = Patron -> Patron -> Patron
type Par = (Number, Number)

versoDeEstrofa :: Number -> Estrofa -> Verso
versoDeEstrofa pos estrofa = estrofa !! (pos - 1)

antepenultimaVocal :: Palabra -> Char
antepenultimaVocal palabra = (reverse . filtrarVocales) palabra !! 2

esEsdrujula :: Palabra -> Bool
esEsdrujula = tieneTilde . antepenultimaVocal

ultimasPalabrasDeEstrofa :: Estrofa -> [Palabra]
ultimasPalabrasDeEstrofa = map ultimaPalabraDeVerso

primerasPalabrasDeEstrofa :: Estrofa -> [Palabra]
primerasPalabrasDeEstrofa = map primerPalabraDeVerso

todasSonEsdrujulas :: [Palabra] -> Bool
todasSonEsdrujulas = all esEsdrujula

todasSonIguales :: [Palabra] -> Bool
todasSonIguales [] = True
todasSonIguales [x] = True
todasSonIguales (p:y:ps)
    |p == y = todasSonIguales (y:ps)
    |otherwise = False

conjugacionEnCadena :: Estrofa -> Bool
conjugacionEnCadena [] = True
conjugacionEnCadena [x] = True
conjugacionEnCadena (x:y:xs)
    | conjugacionDeVersos x y = conjugacionEnCadena (y:xs)
    | otherwise = False 


patronSimple :: Par -> Patron
patronSimple (pos1, pos2) estrofa = 
    cumplen ultimaPalabraDeVerso esRima (versoDeEstrofa pos1 estrofa) (versoDeEstrofa pos2 estrofa)

patronEsdrujula :: Patron
patronEsdrujula = todasSonEsdrujulas

patronAnafora :: Patron
patronAnafora = todasSonIguales . primerasPalabrasDeEstrofa

patronCadena :: Patron
patronCadena = conjugacionEnCadena

patronCombinaDos :: PatronCombinado 
patronCombinaDos pat1 pat2 estrofa = pat1 estrofa && pat2 estrofa


aabb :: Patron
aabb = patronCombinaDos (patronSimple (1,2)) (patronSimple(3,4))

abab :: Patron
abab = patronCombinaDos (patronSimple (1,3)) (patronSimple(2,4))

abba :: Patron
abba = patronCombinaDos (patronSimple (1,4)) (patronSimple(2,3))

hardcore :: Patron
hardcore = patronCombinaDos patronCadena patronEsdrujula

{-
    En el caso del patron hardcore seria imposible saber si cumple con el patron ya que este analiza que primero se cumpla la
    condicion de rimas en cadena y que todas las ultimas palabras de los versos terminen en esdrujula, por lo que si una estrofa
    tiene infinitos versos no hay manera de que los pueda analizar todos ya que son infinitos. Diferente es el caso de
    aabb ya que este solo analiza los primeros 4 versos de la estrofa, por lo que si haskell ya obtiene una respuesta mediante
    esos 4 versos, arrojara el resultado y listo. Esto lo hace gracias a que trabaja con lazy evaluation, donde no necesita
    evualuar todo, sino solamente lo necesario.
-}