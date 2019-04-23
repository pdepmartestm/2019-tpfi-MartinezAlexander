import Text.Show.Functions
-- TESOROS PIRATAS 
type Nombre = String
type Valor = Integer
type Tesoro = (Nombre,Valor)
type Botin = [Tesoro]
type Pirata = (Nombre,Botin)

-- funciones de manejo de tesoros 
nombreTesoro :: Tesoro->Nombre
nombreTesoro (nombre,valor) = nombre
valorTesoro :: Tesoro->Valor
valorTesoro (nombre,valor) = valor
esTesoroValioso :: Tesoro->Bool
esTesoroValioso tesoro = ((>100).valorTesoro)tesoro
esNombreDeTesoro :: Nombre->Tesoro->Bool
esNombreDeTesoro nombre tesoro = ((==nombre).nombreTesoro) tesoro

ejemploTesoro = ("Oro",500)

-- funciones de manejo de botines
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

ejemploBotin1 = [("Sombrero",250),("Diamante",5000),("Zafiro",1000),("Reloj",105)]
ejemploBotin2 = [("Acero",150),("Petroleo",5000),("Plata",1000),("Corona de oro",980)]
ejemploBotin3 = [("Carbon",150)]

-- funciones de manejo de piratas 
nombrePirata :: Pirata->Nombre
nombrePirata (nombre,botin) = nombre
botinPirata :: Pirata->Botin
botinPirata (nombre,botin) = botin
cantidadTesorosDePirata :: Pirata->Int
cantidadTesorosDePirata pirata = cantidadTesorosBotin (botinPirata pirata)
pirataAfortunado :: Pirata->Bool
pirataAfortunado pirata = (valorBotin (botinPirata pirata)) >10000
valorTesoroMasValiosoDelPirata :: Pirata->Valor
valorTesoroMasValiosoDelPirata pirata = valorTesoroMasValiosoBotin (botinPirata pirata)
pirataAdquiereTesoro :: Pirata->Tesoro->Pirata
pirataAdquiereTesoro pirata tesoro = (nombrePirata pirata, agregarTesoroABotinn True (botinPirata pirata) tesoro)
pirataPierdeTesorosValioso :: Pirata->Pirata
pirataPierdeTesorosValioso pirata = (nombrePirata pirata , quitarTesorosValiosos (botinPirata pirata))
pirataPierdeTesorosPorNombre :: Pirata->Nombre->Pirata
pirataPierdeTesorosPorNombre pirata nombre = (nombrePirata pirata, quitarTesorosPorNombre (botinPirata pirata) (esNombreDeTesoro nombre))
esNombreDePirata :: Nombre->Pirata->Bool
esNombreDePirata nombre pirata = (nombrePirata pirata) == nombre


ejemploPirata1 = ("Jack Sparrow", ejemploBotin1)
ejemploPirata2 = ("Annie Bonny", ejemploBotin2)

-- TEMPORADA DE SAQUEOS
saquearTesorosValiosos :: Tesoro->Bool
saquearTesorosValiosos tesoro = esTesoroValioso tesoro
saquearTesorosEspecificos :: Nombre->Tesoro->Bool
saquearTesorosEspecificos nombre tesoro = esNombreDeTesoro nombre tesoro
saquearDeCorazon :: Tesoro->Bool
saquearDeCorazon tesoro = False
saquearComoSea :: Nombre->Tesoro->Bool
saquearComoSea nombre tesoro = (saquearTesorosValiosos tesoro) || (saquearTesorosEspecificos nombre tesoro)

saquear :: (Tesoro->Bool)->Tesoro->Pirata->Pirata
saquear formaDeSaqueo tesoro pirata  = (nombrePirata pirata, agregarTesoroABotinn (formaDeSaqueo tesoro) (botinPirata pirata) (tesoro))

-- NAVEGANDO LOS SIETE MARES
type Tripulacion = [Pirata]
type FormaDeSaquear = (Tesoro->Bool)
type Barco = (Nombre,Tripulacion,FormaDeSaquear)

-- funciones de manejo de tripulacion
agregarPirataATripulacion :: Tripulacion->Pirata->Tripulacion
agregarPirataATripulacion tripulacion pirata = pirata:tripulacion
quitarPirataDeTripulacion :: Tripulacion->(Pirata->Bool)->Tripulacion
quitarPirataDeTripulacion tripulacion esNombre = filter (not.esNombre) tripulacion

-- funciones de manejo de barcos
nombreBarco :: Barco->Nombre
nombreBarco (nombre,tripulacion,formaDeSaqueo) = nombre
tripulacionBarco :: Barco->Tripulacion
tripulacionBarco (nombre,tripulacion,formaDeSaqueo) = tripulacion
formaDeSaqueoBarco :: Barco->FormaDeSaquear
formaDeSaqueoBarco (nombre,tripulacion,formaDeSaqueo) = formaDeSaqueo
incorporarPirataABarco :: Barco->Pirata->Barco
incorporarPirataABarco barco pirata = (nombreBarco barco, agregarPirataATripulacion (tripulacionBarco barco) pirata, formaDeSaqueoBarco barco)
pirataAbandonaBarco :: Barco->Nombre->Barco
pirataAbandonaBarco barco nombre = (nombreBarco barco, quitarPirataDeTripulacion (tripulacionBarco barco) (esNombreDePirata nombre), formaDeSaqueoBarco barco)

type Isla = (Nombre,Tesoro)
nombreIsla :: Isla->Nombre
nombreIsla (nombre,_ ) = nombre
tesoroTipicoIsla :: Isla->Tesoro
tesoroTipicoIsla ( _ , tesoro) = tesoro
anclarEnIslaDeshabitada :: Barco->Isla->Barco
anclarEnIslaDeshabitada barco isla = (nombreBarco barco, map (saquear (formaDeSaqueoBarco barco) (tesoroTipicoIsla isla)) (tripulacionBarco barco), formaDeSaqueoBarco barco)

type Ciudad = (Nombre, Botin)
botinCiudad :: Ciudad->Botin
botinCiudad (_,botin) = botin
atacarCiudad :: Barco->Ciudad->Barco
atacarCiudad barco ciudad = (nombreBarco barco, zipWith (saquear (formaDeSaqueoBarco barco)) (botinCiudad ciudad) (tripulacionBarco barco), formaDeSaqueoBarco barco)

-- la tripulacion del barco1 cambia de barco y asesina a todos los tripulantes del barco2 tirandolos al agua con todo y tesoros y adapta su forma de saqueo
abordarOtroBarcoEnAltaMar :: Barco->Barco->Barco
abordarOtroBarcoEnAltaMar barco1 barco2 = (nombreBarco barco2, tripulacionBarco barco1, formaDeSaqueoBarco barco2)

ejemploIsla = ("Isla Tortuga",("Platino",50))
ejemploCiudad = ("Buenos Aires", ejemploBotin3)
ejemploFormaSaqueo = (saquearTesorosValiosos)
ejemploBarco = ("Perla Negra", [ejemploPirata1, ejemploPirata2], ejemploFormaSaqueo)