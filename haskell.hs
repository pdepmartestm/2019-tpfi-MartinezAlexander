import Text.Show.Functions
type Nombre = String
type Valor = Integer
------------TESOROS----------------
data Tesoro = UnTesoro {nombreTesoro :: Nombre, valorTesoro :: Valor}
instance Show Tesoro where show tes = (nombreTesoro tes)++" de "++(show (valorTesoro tes))++" dolares"
--ejemplo
tesoro1 = UnTesoro {nombreTesoro = "Oro", valorTesoro = 2500}
tesoro2 = UnTesoro {nombreTesoro = "Diamante", valorTesoro = 4000}
tesoro3 = UnTesoro {nombreTesoro = "Carbon", valorTesoro = 150}
--funciones
esTesoroValioso :: Tesoro->Bool
esTesoroValioso tesoro = ((>100).valorTesoro)tesoro
esNombreDeTesoro :: Nombre->Tesoro->Bool
esNombreDeTesoro nombre tesoro = ((==nombre).nombreTesoro) tesoro

-------------BOTIN---------------
type Botin = [Tesoro]
--ejemplo
botin1 = [tesoro1,tesoro2]
botin2 = []
botin3 = [tesoro3]
--funciones
valorBotin :: Botin->Valor
valorBotin botin = sum (map (valorTesoro) botin)
valorTesoroMasValiosoBotin :: Botin->Valor
valorTesoroMasValiosoBotin botin = maximum (map (valorTesoro) botin)
cantidadTesorosBotin :: Botin->Int
cantidadTesorosBotin botin = length botin
agregarTesoroABotinn saquear botin tesoro | (saquear == True) =  tesoro:botin
agregarTesoroABotinn saquear botin tesoro | (saquear == False) = botin
quitarTesorosValiosos :: Botin->Botin
quitarTesorosValiosos botin = filter (not.esTesoroValioso) botin
quitarTesorosPorNombre :: Botin->(Tesoro->Bool)->Botin
quitarTesorosPorNombre botin esNombre = filter (not.esNombre) botin

---------------PIRATA-------------
data Pirata = UnPirata {nombrePirata :: Nombre, botinPirata :: Botin}
instance Show Pirata where show pir = (nombrePirata pir)++", "++(show (botinPirata pir))
--ejemplo
pirata1 = UnPirata {nombrePirata = "Jack Sparrow", botinPirata = botin1}
pirata2 = UnPirata {nombrePirata = "Annie Bonny", botinPirata = botin2}
-- funciones 
cantidadTesorosDePirata :: Pirata->Int
cantidadTesorosDePirata pirata = cantidadTesorosBotin (botinPirata pirata)
pirataAfortunado :: Pirata->Bool
pirataAfortunado pirata = (valorBotin (botinPirata pirata)) >10000
valorTesoroMasValiosoDelPirata :: Pirata->Valor
valorTesoroMasValiosoDelPirata pirata = valorTesoroMasValiosoBotin (botinPirata pirata)
pirataAdquiereTesoro :: Pirata->Tesoro->Pirata
pirataAdquiereTesoro pirata tesoro = UnPirata {nombrePirata = (nombrePirata pirata), botinPirata = (agregarTesoroABotinn True (botinPirata pirata) tesoro)}
pirataPierdeTesorosValioso :: Pirata->Pirata
pirataPierdeTesorosValioso pirata = UnPirata {nombrePirata = (nombrePirata pirata), botinPirata = quitarTesorosValiosos (botinPirata pirata)}
pirataPierdeTesorosPorNombre :: Pirata->Nombre->Pirata
pirataPierdeTesorosPorNombre pirata nombre = UnPirata {nombrePirata = (nombrePirata pirata),botinPirata = quitarTesorosPorNombre (botinPirata pirata) (esNombreDeTesoro nombre)}
esNombreDePirata :: Nombre->Pirata->Bool
esNombreDePirata nombre pirata = (nombrePirata pirata) == nombre

-- -------TEMPORADA DE SAQUEOS--------
saquearTesorosValiosos :: Tesoro->Bool
saquearTesorosValiosos tesoro = esTesoroValioso tesoro
saquearTesorosEspecificos :: Nombre->Tesoro->Bool
saquearTesorosEspecificos nombre tesoro = esNombreDeTesoro nombre tesoro
saquearDeCorazon :: Tesoro->Bool
saquearDeCorazon tesoro = False
saquearComoSea :: Nombre->Tesoro->Bool
saquearComoSea nombre tesoro = (saquearTesorosValiosos tesoro) || (saquearTesorosEspecificos nombre tesoro)
saquear :: (Tesoro->Bool)->Tesoro->Pirata->Pirata
saquear formaDeSaqueo tesoro pirata  = UnPirata {nombrePirata = (nombrePirata pirata), botinPirata = agregarTesoroABotinn (formaDeSaqueo tesoro) (botinPirata pirata) (tesoro)}

-----------TRIPULACION-----------------
type Tripulacion = [Pirata]
--ejemplo
tripulacion1 = [pirata1,pirata2]
-- funciones
agregarPirataATripulacion :: Tripulacion->Pirata->Tripulacion
agregarPirataATripulacion tripulacion pirata = pirata:tripulacion
quitarPirataDeTripulacion :: Tripulacion->(Pirata->Bool)->Tripulacion
quitarPirataDeTripulacion tripulacion esNombre = filter (not.esNombre) tripulacion

-----------FORMA DE SAQUEO-----------------
type FormaSaqueo = (Tesoro->Bool)
--ejemplo
saqueoValioso = (saquearTesorosValiosos)
saqueoEspecifico = (saquearTesorosEspecificos "Oro")
saqueoCorazon = (saquearDeCorazon)
saqueOSiempre = (saquearComoSea "Diamante")

--------------BARCO--------------
data Barco = UnBarco {nombreBarco :: Nombre, tripulacionBarco :: Tripulacion, formaDeSaqueoBarco :: FormaSaqueo} 
instance Show Barco where show bar = "Barco: " ++ (nombreBarco bar) ++ ", Tripulacion: " ++ (show(tripulacionBarco bar)) ++ ", " ++ (show (formaDeSaqueoBarco bar))
--ejemplo
barco1 = UnBarco {nombreBarco = "Perla Negra", tripulacionBarco = tripulacion1, formaDeSaqueoBarco = saqueoValioso}
-- funciones de manejo de barcos
incorporarPirataABarco :: Barco->Pirata->Barco
incorporarPirataABarco barco pirata = UnBarco {nombreBarco = (nombreBarco barco),tripulacionBarco = (agregarPirataATripulacion (tripulacionBarco barco) pirata), formaDeSaqueoBarco = (formaDeSaqueoBarco barco)}
pirataAbandonaBarco :: Barco->Nombre->Barco
pirataAbandonaBarco barco nombre = UnBarco {nombreBarco = (nombreBarco barco), tripulacionBarco = (quitarPirataDeTripulacion (tripulacionBarco barco) (esNombreDePirata nombre)),formaDeSaqueoBarco = (formaDeSaqueoBarco barco)} 

---------------ISLA--------------
data Isla = UnaIsla {nombreIsla :: Nombre, tesoroTipicoIsla :: Tesoro} deriving Show
--ejemplo
isla1 = UnaIsla {nombreIsla = "Isla Tortuga", tesoroTipicoIsla = tesoro2}
--funciones 
anclarEnIslaDeshabitada :: Barco->Isla->Barco
anclarEnIslaDeshabitada barco isla = UnBarco {nombreBarco = (nombreBarco barco), tripulacionBarco = (map (saquear (formaDeSaqueoBarco barco) (tesoroTipicoIsla isla)) (tripulacionBarco barco)),formaDeSaqueoBarco = (formaDeSaqueoBarco barco)}

--------------CIUDAD---------------
data Ciudad = UnaCiudad {nombreCiudad :: Nombre, botinCiudad :: Botin} deriving Show
--ejemplo
ciudad1 = UnaCiudad {nombreCiudad = "La Plata", botinCiudad = botin3}
--funciones
atacarCiudad :: Barco->Ciudad->Barco
atacarCiudad barco ciudad = UnBarco {nombreBarco = (nombreBarco barco), tripulacionBarco = (zipWith (saquear (formaDeSaqueoBarco barco)) (botinCiudad ciudad) (tripulacionBarco barco)), formaDeSaqueoBarco = (formaDeSaqueoBarco barco)}

-- la tripulacion del barco1 cambia de barco y asesina a todos los tripulantes del barco2 tirandolos al agua con todo y tesoros y adapta su forma de saqueo
abordarOtroBarcoEnAltaMar :: Barco->Barco->Barco
abordarOtroBarcoEnAltaMar barco1 barco2 = UnBarco {nombreBarco = (nombreBarco barco2),tripulacionBarco =  (tripulacionBarco barco1), formaDeSaqueoBarco = (formaDeSaqueoBarco barco2)}