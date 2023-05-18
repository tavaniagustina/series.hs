data Serie = Serie {
    nombreSerie :: String,
    actores :: [Actor],
    presupuesto :: Int,
    temporadas :: Int,
    raiting :: Int,
    cancelada :: Bool
}

data Actor = Actor {
    nombreActor :: String,
    sueldo :: Int,
    restricciones :: [String]
}

--------------
-- Punto 01 --
--------------

estaEnRojo :: Serie -> Bool
estaEnRojo unaSerie = (presupuesto unaSerie) > cobranActores (actores unaSerie)

cobranActores :: [Actor] -> Int
cobranActores = sum . map sueldo

esProblematica :: Serie -> Bool
esProblematica = (>3) . (cantidadDeRestricciones 1) 

cantidadDeRestricciones :: Int -> Serie -> Int
cantidadDeRestricciones num unaSerie = length $ filter ( (> num) . length . restricciones) (actores unaSerie)

--------------
-- Punto 02 --
--------------

type Produccion = Serie -> Serie 

conFavoritismo :: [Actor] -> Produccion
conFavoritismo actoresFavoritos = reemplazarActores actoresFavoritos . (eliminarActores 2)

eliminarActores :: Int -> Serie -> Serie
eliminarActores num = mapActoresSeries (drop num)

reemplazarActores :: [Actor] -> Serie -> Serie
reemplazarActores actoresFavoritos = mapActoresSeries (++ actoresFavoritos)

mapActoresSeries :: ([Actor] -> [Actor]) -> Serie -> Serie
mapActoresSeries unaFuncion unaSerie = unaSerie { actores = unaFuncion (actores unaSerie) }

timBurton = conFavoritismo [jonnyDepp, helenaBonhamCarter]

jonnyDepp :: Actor
jonnyDepp = Actor "johnny depp" 20000000 []

helenaBonhamCarter :: Actor
helenaBonhamCarter = Actor "helena bonham carter" 15000000 []

gatopardeitor :: Produccion
gatopardeitor unaSerie = unaSerie

estireitor :: Produccion
estireitor =  mapTemporadas (*2) 

mapTemporadas :: (Int-> Int) -> Serie -> Serie
mapTemporadas unaFuncion unaSerie = unaSerie { temporadas = unaFuncion (temporadas unaSerie) }

desespereitor :: Produccion
desespereitor = gatopardeitor . estireitor 

canceleitor :: Int -> Produccion
canceleitor unaCifra unaSerie
    | estaEnRojo unaSerie || (raiting unaSerie) > unaCifra = mapCancelada (const True) unaSerie
    | otherwise                                            = unaSerie

mapCancelada :: (Bool -> Bool) -> Serie -> Serie
mapCancelada unaFuncion unaSerie = unaSerie { cancelada = unaFuncion (cancelada unaSerie) }

--------------
-- Punto 03 --
--------------

bienestarSerie :: Serie -> Int
bienestarSerie unaSerie 
    | (cancelada unaSerie) = 0 
    | otherwise            = (bienestarTemporadas unaSerie) + (bienestarActores unaSerie)

bienestarTemporadas :: Serie -> Int
bienestarTemporadas unaSerie
    | (temporadas unaSerie) > 4 = 5
    | otherwise                 = (10 - (temporadas unaSerie)) * 2

bienestarActores :: Serie -> Int
bienestarActores unaSerie
    | (length $ actores unaSerie) < 10 = 3
    | otherwise                      = 10 - (cantidadDeRestricciones 2 unaSerie)  

--------------
-- Punto 04 --
--------------

productorMasEfectivo :: [Serie] -> [Produccion] -> [Serie]
productorMasEfectivo series productores = map (masDinero productores) series

masDinero :: [Produccion] -> Serie -> Serie
masDinero (x:[]) unaSerie = x unaSerie
masDinero (x:xs) unaSerie
    | bienestarSerie (x unaSerie) > bienestarSerie (head xs & unaSerie) = x unaSerie
    | otherwise                                                         = masDinero xs unaSerie

--------------
-- Punto 05 --
--------------

