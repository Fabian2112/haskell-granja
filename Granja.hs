{-# OPTIONS_GHC -Wno-missing-fields #-}
import Data.List ()
import Text.Show.Functions ()


luci :: Animal
luci = Animal "luci" 30 10 True 80 120

orejon :: Animal
orejon = Animal "orejon" 50 15 False 30 20

budi :: Animal
budi = Animal "budi" 10 10 True 0 20

mini :: Animal
mini = Animal "mini" 0 10 False 10 500

shovi :: Animal
shovi = Animal "shovi" 80 30 True 120 1200



carlos :: Veterinario
carlos = Veterinario {
   vitaminas = 2,
   diasDeRecuperacionVet = 40,
   costoVet = 500
}


data Animal = Animal {
   nombre :: String,
   peso :: Double,
   edad :: Double,
   estaEnfermo :: Bool,
   diasDeRecuperacion :: Double,
   costo :: Double
} deriving (Show)


data Veterinario = Veterinario {
    diagnostico :: Diagnostico,
    diasDeRecuperacionVet :: Double,
    costoVet :: Double,
    vitaminas :: Double
} deriving (Show)
    

type Diagnostico = Animal -> Animal

laPasoMal :: Animal -> Bool
laPasoMal animal = (>30) $ diasDeRecuperacion animal


nombreFalopa :: Animal -> Bool
nombreFalopa animal = (=='i') . last $ nombre animal

type KilosComida = Double

engorde :: KilosComida -> Animal -> Animal

engorde kilosComida animal = Animal {
   nombre = nombre animal,
   peso = peso animal + min 5 (kilosComida/2),
   edad = edad animal,
   estaEnfermo = estaEnfermo animal,
   diasDeRecuperacion = diasDeRecuperacion animal,
   costo = costo animal
}


type Revisacion = Veterinario -> Animal -> Animal


revisacion :: Revisacion
revisacion veterinario animal = Animal {
   nombre = nombre animal,
   peso = (+) 2 $ peso animal,
   edad = edad animal,
   estaEnfermo = False,
   diasDeRecuperacion = diasDeRecuperacionVet veterinario,
   costo = costoVet veterinario
}


festejoCumple :: Animal -> Animal
festejoCumple animal = Animal {
   nombre = nombre animal,
   edad = (+) (edad animal) 1,
   peso = (-) (peso animal) 1,
   estaEnfermo = estaEnfermo animal,
   diasDeRecuperacion = diasDeRecuperacion animal,
   costo = costo animal
}


type PesoLimite = Double

chequeoDePeso :: PesoLimite -> Animal -> Animal

chequeoDePeso pesoLimite animal
   | peso animal < pesoLimite = Animal{nombre = nombre animal,
   edad = edad animal,
   peso = peso animal,
   estaEnfermo = True,
   diasDeRecuperacion = diasDeRecuperacion animal,
   costo = costo animal}
   | otherwise = Animal{nombre = nombre animal,
   edad = edad animal,
   peso = peso animal,
   estaEnfermo = False,
   diasDeRecuperacion = diasDeRecuperacion animal,
   costo = costo animal}


   
type Actividad = Animal -> Animal   

type Proceso = [Actividad]

proceso1 :: Proceso
proceso1 = [engorde 8, festejoCumple, chequeoDePeso 20]

elProceso :: Proceso -> Animal -> Animal
elProceso proceso animal = foldr ($) animal proceso


mejoraONoMejora :: Proceso -> Animal -> Bool
mejoraONoMejora [] animal = True
mejoraONoMejora (x:xs) animal = (&&) ((<=) (peso $ x animal) (peso animal + 3)) ((>) (peso $ x animal) (peso animal)) && mejoraONoMejora xs animal


type Animales = [Animal]

animales1 :: Animales
animales1 = [luci,orejon,budi,mini,shovi]

giveMeThree :: Animales -> Animales
giveMeThree = take 3 . filter nombreFalopa



--seria posible por el lazy evaluation y la funcion "take" que solo toma los 3 primeros de cualquier lista que le deseemos pasar sin importarle la cantidad de elementos que esta misma posea

