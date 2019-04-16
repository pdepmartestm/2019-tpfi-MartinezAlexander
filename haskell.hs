--Tesoros piratas
type Tesoro = (String,Integer)
type Botin = [Tesoro]
type Pirata = (String,Botin)

-- Ejemplos piratas y botin
ejemploBotin = [("Frasco de Arena",5) , ("Brujula",10000)]
ejemploPirata = ("Jack Sparrow" , [("Frasco de Arena",0) , ("Brujula",10000)])
ejemploPirata2 = ("David Jones" , [("Cajita musical",1)])
ejemploPirata3 = ("Anne Bonny", [("Doblones",100) , ("Frasco de Arena",1)])


--funciones de tesoro
nombreTesoro :: Tesoro->String
nombreTesoro (a,b) = a
valorTesoro :: Tesoro->Integer
valorTesoro (a,b) = b
noEsNombreDeTesoro :: String->Tesoro->Bool
noEsNombreDeTesoro n t = (nombreTesoro t) /= n
noEsTesoroValioso :: Tesoro->Bool
noEsTesoroValioso t = (valorTesoro t) <=100


--funciones de Botin
valorBotin :: Botin->Integer
valorBotin b = sum (map (valorTesoro) b)
agregarTesoroABotin :: Botin->Tesoro->Botin
agregarTesoroABotin b t = t:b
quitarTesorosValiosos :: Botin->Botin
quitarTesorosValiosos b =(filter (noEsTesoroValioso) b)
quitarTesorosPorNombre :: Botin->String->Botin
quitarTesorosPorNombre b n = (filter (noEsNombreDeTesoro n) b)


--funciones de pirata
nombrePirata :: Pirata->String
nombrePirata (a,b) = a
noEsNombreDePirata :: String->Pirata->Bool
noEsNombreDePirata n p = (nombrePirata p) /= n
botinPirata :: Pirata->Botin
botinPirata (a,b) = b
cantidadTesorosDePirata :: Pirata->Int
cantidadTesorosDePirata p = length(botinPirata p)
pirataAfortunado :: Pirata->Bool
pirataAfortunado p = (valorBotin(botinPirata p)) > 10000
tesoroMasValiosoPirata :: Pirata->Integer
tesoroMasValiosoPirata p = maximum (map (valorTesoro) (botinPirata p))
adquirirTesoro ::Pirata->Tesoro->Pirata
adquirirTesoro p t = (nombrePirata p, agregarTesoroABotin (botinPirata p) t)
perderTesorosValiosos :: Pirata->Pirata
perderTesorosValiosos p = (nombrePirata p, quitarTesorosValiosos (botinPirata p))
perderTesorosPorNombre :: Pirata->String->Pirata
perderTesorosPorNombre p n = (nombrePirata p, quitarTesorosPorNombre (botinPirata p) n)


-- Temporada de saqueos 

saquearTesorosValiosos :: String->Tesoro->Bool
saquearTesorosValiosos n t = not(noEsTesoroValioso t)
saquearTesorosEspecificos :: String->Tesoro->Bool
saquearTesorosEspecificos n t= not (noEsNombreDeTesoro n t)
saquearNada :: String->Tesoro->Bool
saquearNada n t = False
saquearCualquierEstilo :: String->Tesoro->Bool
saquearCualquierEstilo n t = (saquearTesorosValiosos n t) || (saquearTesorosEspecificos n t)

concretarSaqueo :: Bool->Pirata->Tesoro->Pirata
concretarSaqueo True p t = adquirirTesoro p t
concretarSaqueo False p t = p

saquear :: Pirata->Bool->Tesoro->Pirata
saquear p fs t = (concretarSaqueo fs p t)

-- Navegando los siete mares

type FormaSaqueo = (Integer,String)
type Tripulacion = [Pirata]
type Barco = (String,Tripulacion,FormaSaqueo)

-- Tipo de saqueo
formaSaquear:: FormaSaqueo->Tesoro->Bool
formaSaquear (0,cl) t = (saquearTesorosValiosos cl t)
formaSaquear (1,cl) t = (saquearTesorosEspecificos cl t)
formaSaquear (2,cl) t = (saquearNada cl t)
formaSaquear (3,cl) t = (saquearCualquierEstilo cl t)

-- funciones de tripulacion
agregarATripulacion :: Tripulacion->Pirata->Tripulacion
agregarATripulacion t p = p:t
eliminarDeTripulacion :: Tripulacion->String->Tripulacion
eliminarDeTripulacion t n = (filter (noEsNombreDePirata n) t) 

-- funciones de barco
obtenerNombreBarco :: Barco->String
obtenerNombreBarco (n,t,fs) = n
obtenerTripulacion :: Barco->Tripulacion
obtenerTripulacion (n,t,fs) = t
obtenerFormaSaqueo :: Barco->FormaSaqueo
obtenerFormaSaqueo (n,t,fs) = fs
abordarPirataABarco :: Barco->Pirata->Barco
abordarPirataABarco b p = (obtenerNombreBarco b, (agregarATripulacion (obtenerTripulacion b) p), obtenerFormaSaqueo b)
quitarPirataDeBarco :: Barco->String->Barco
quitarPirataDeBarco b n = (obtenerNombreBarco b, (eliminarDeTripulacion (obtenerTripulacion b) n), obtenerFormaSaqueo b)

-- ejemplo barco y forma saqueo
ejemploSaqueo = (1,"Oro")
ejemploBarco = ( "Perla Negra" , [ ejemploPirata , ejemploPirata2 ] , ejemploSaqueo )
ejemploTripulacion = [ ejemploPirata , ejemploPirata2 , ejemploPirata3 ]

-- Anclar en isla deshabitada 

type Isla = (String,Tesoro)

obtenerTesoroIsla :: Isla->Tesoro
obtenerTesoroIsla (n,t) = t
obtenerNombreIsla :: Isla->String
obtenerNombreIsla (n,t) = n

agregarSaqueo :: Bool->Tesoro->Pirata->Pirata
agregarSaqueo fs t p = saquear p fs t
agregarMultitud :: Tripulacion->Tesoro->FormaSaqueo->Tripulacion
agregarMultitud tri te fs = (map (agregarSaqueo (formaSaquear fs te) te) tri)

desembarcarEnIsla :: Barco->Isla->Barco
desembarcarEnIsla b i = (obtenerNombreBarco b, agregarMultitud (obtenerTripulacion b) (obtenerTesoroIsla i) (obtenerFormaSaqueo b) , obtenerFormaSaqueo b)

-- Atacar ciudad 
type Ciudad = (String,Botin)

obtenerBotinCiudad :: Ciudad->Botin
obtenerBotinCiudad (n,b) = b


type Combinacion = (Pirata,Tesoro)

juntarTesorosPiratas :: Tripulacion->Botin->[Combinacion]
juntarTesorosPiratas t b = zip t b
comparacion :: [Combinacion]->Tripulacion->Bool
comparacion c t = (length c) < (length t)
resultadoComparar :: Bool->[Combinacion]->Tripulacion->Tripulacion
resultadoComparar True c t = (drop (length c) t)
resultadoComparar False c t = (drop (length t) t)
agregarTesoroSiSePuede :: FormaSaqueo->Combinacion->Pirata
agregarTesoroSiSePuede fs (p,t) = agregarSaqueo (formaSaquear fs t) t p

tripulacionYtesoros :: Tripulacion->Botin->FormaSaqueo->Tripulacion
tripulacionYtesoros t b fs = (map (agregarTesoroSiSePuede fs) (juntarTesorosPiratas t b))

--tripulacionSinTesoros :: Tripulacion->Botin->Tripulacion
--tripulacionSinTesoros t b =(resultadoComparar (comparacion (juntarTesorosPiratas t b) t) (juntarTesorosPiratas t b) t)

asaltarCiudad :: Ciudad->Barco->Barco
--asaltarCiudad c b = (obtenerNombreBarco b , (tripulacionYtesoros (obtenerTripulacion b) (obtenerBotinCiudad c) (obtenerFormaSaqueo b)) ++ (tripulacionSinTesoros (obtenerTripulacion b) (obtenerBotinCiudad c) ) , obtenerFormaSaqueo b) 
asaltarCiudad c b = (obtenerNombreBarco b , (tripulacionYtesoros (obtenerTripulacion b) (obtenerBotinCiudad c) (obtenerFormaSaqueo b)) , obtenerFormaSaqueo b) 
-- ejemplos para asaltar ciudad

ejemploPirata11 = ("Jack Sparrow" , [("Sombrero",200) , ("Brujula",10000)])
ejemploPirata12 = ("Anne Bonny" , [("Oro",350) , ("Diamante",10000)])
ejemploTripulacion11 = [ ejemploPirata11 , ejemploPirata12 ]
ejemploSaqueo11 = (0," ")
ejemploBarco11 = ("Perla Negra" , ejemploTripulacion11 , ejemploSaqueo11 )
ejemploBarco12 = ("Holandes errante" , ejemploTripulacion , ejemploSaqueo )


ejemploTesoro11 = ("Zafiro",950)
ejemploTesoro12 = ("Reloj",1500)
ejemploTesoro13 = ("Piedra",50)
ejemploBotin11 = [ ejemploTesoro11 , ejemploTesoro12 , ejemploTesoro13]
ejemploBotin12 = [ ejemploTesoro11 , ejemploTesoro13]
ejemploBotin13 = [ ejemploTesoro11 ]

ejemploCiudad11 = ("Buenos Aires", ejemploBotin11 )
ejemploCiudad12 = ("Buenos Aires", ejemploBotin12 )
ejemploCiudad13 = ("Buenos Aires", ejemploBotin13 )

abordarOtroBarcoEnAltaMar :: Barco->Barco->Barco
abordarOtroBarcoEnAltaMar b1 b2 = (obtenerNombreBarco b2, obtenerTripulacion b1 , obtenerFormaSaqueo b1)