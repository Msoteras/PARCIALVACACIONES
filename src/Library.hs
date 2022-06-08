module Library where
import PdePreludat

--Punto 1
data Turista = UnTurista {
    cansancio :: Number,
    stress :: Number,
    viajaSolo :: Bool,
    idiomas :: [String]
} deriving (Show,Eq)

ana = UnTurista{cansancio=0, stress=21,viajaSolo=False,idiomas=["Español"]}
beto = UnTurista{cansancio=15,stress=15,viajaSolo=True,idiomas=["Aleman"]}
cathi = UnTurista{cansancio=15,stress=15,viajaSolo=True,idiomas=["Aleman","Catalan"]}

--Punto 2

type Excursion = Turista -> Turista

irALaPlaya :: Excursion
irALaPlaya turista | viajaSolo turista = turista{cansancio = reducir (cansancio turista) 5}
                   | otherwise = turista{stress = reducir (stress turista) 1}

apreciarAlgunElemento :: String -> Excursion
apreciarAlgunElemento element turista = turista{stress = reducir (stress turista) (length element) }

salirAHablarIdioma :: String -> Excursion
salirAHablarIdioma unIdioma turista= turista {viajaSolo=False, idiomas = unIdioma:(idiomas turista)}

caminar :: Number -> Excursion
caminar minutos turista = turista{cansancio = aumentar (cansancio turista) (intensidad minutos) , stress = reducir (stress turista) (intensidad minutos) }

intensidad :: Number -> Number
intensidad minutos = minutos/4

paseoEnBarco :: String -> Excursion
paseoEnBarco marea turista 
        | marea == "fuerte" = turista {cansancio = aumentar (cansancio turista) 10 , stress = aumentar (stress turista) 6 }
        | marea == "moderada" = turista
        | marea == "tranquila" =foldl (\turista excursion -> excursion turista) turista [caminar 10, apreciarAlgunElemento "mar", salirAHablarIdioma "Aleman"]


reducir :: Number -> Number -> Number
reducir unidades1 unidades2 = unidades1 - unidades2

aumentar :: Number -> Number -> Number
aumentar unidades1 unidades2 = unidades1 + unidades2

-- Item a
hacerUnaExcursion :: Excursion -> Turista -> Turista
hacerUnaExcursion excursion turista = (excursion turista){stress = stress (excursion turista) - stress (excursion turista)*0.1 }
--o el porcetaje con div porcentaje 10

--Item b
type Indice = Number

deltaSegun :: (a -> Number) -> a -> a -> Indice
deltaSegun f algo1 algo2 = f algo1 - f algo2

deltaExcursionSegun ::  (Turista -> Number) -> Turista -> Excursion -> Indice
deltaExcursionSegun indice turista excursion = deltaSegun indice turista (excursion turista)

--Item c

esEducativa :: Excursion -> Turista -> Bool
esEducativa excursion turista = (deltaExcursionSegun ((length.idiomas)) turista excursion) >= 1

excursionesDesestresantes :: Turista -> [Excursion] -> [Excursion]
excursionesDesestresantes turista excursiones = filter ((>=3).(deltaExcursionSegun stress turista)) excursiones

--Punto 3

type Tour = [Excursion]

completo :: Tour
completo = [caminar 20, apreciarAlgunElemento "cascada", caminar 40, irALaPlaya, salirAHablarIdioma "melmacquiano"]

ladoB :: Excursion -> Tour
ladoB exElegida = [paseoEnBarco "tranquilas", exElegida, caminar 120]

islaVecina :: String -> Tour
islaVecina marea 
            | marea == "fuerte" = [paseoEnBarco marea, apreciarAlgunElemento "lago", paseoEnBarco marea]
            | otherwise = [paseoEnBarco marea, irALaPlaya, paseoEnBarco marea]

--Item A
hacerUnTour :: Turista -> Tour -> Turista
hacerUnTour turista tour = foldl (\turist excursion -> excursion turist) (turista{stress = aumentar (stress turista) (length tour)}) tour

--Item B
algunoEsConvincente :: Turista -> [Tour] -> Bool
algunoEsConvincente turista tours = any (esConvincente turista) tours

esConvincente :: Turista -> Tour -> Bool
esConvincente turista tour = any (estaAcompaniado turista) (excursionesDesestresantes turista tour)
--                                  any (estaAcompaniado turista). excursionesDesestresantes turista

estaAcompaniado :: Turista -> Excursion -> Bool
estaAcompaniado turista excursion = viajaSolo (excursion turista)

--Item c
efectividad :: Tour -> [Turista] -> Number
efectividad tour turistas =  sum (espiritualidadConjunto (filtrarConvincente turistas tour) tour)

filtrarConvincente :: [Turista] -> Tour -> [Turista]
filtrarConvincente turistas tour = filter (flip esConvincente tour) turistas

espiritualidadConjunto :: [Turista] -> Tour -> [Number]
espiritualidadConjunto turistas tour = map (espiritualidad tour) turistas

espiritualidad :: Tour -> Turista -> Number
espiritualidad tour turista = deltaSegun stress turista (hacerUnTour turista tour) + deltaSegun cansancio turista (hacerUnTour turista tour)

-- Punto 4

infinitasPlayas :: Tour
infinitasPlayas = cycle infinitasPlayas

{-Este tour si podria ser convincente para Ana ya que por mas que sea Infinito el tour sserá desestresante luego de una sola excursion y
ella ya está acompñanada, por lo que por uso de lazy evalution no seguiria chequeando otras excrusiones
En cambio con Beto no podriamos saberlo puesto que como él viaja solo e ir a la playa no agrega acompañamiento,
el codigo seguiria ejecutandose en loop ya que estaria en busqueda de la excursion que sea convincente para siempre

item c

Nunca podriamos saber la efectividad del tour ya que esta se obitene una vez las personas terminan de viajar, entonces
no podria calcularse ya que estos turistas nunca terminarian sus infinitas excursiones -}