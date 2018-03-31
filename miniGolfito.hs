import Text.Show.Functions
import Test.Hspec
import Data.List

------------------------------------------------------------------------------------------------------
-- MINIGOLFITO

data Participante = CParticipante {
  nombre :: String, 
  padre :: String,
  fuerza :: Float,
  precision :: Float
} deriving (Show, Eq)

data Tiro = CTiro {
  velocidad :: Float,
  precisionTiro :: Float,
  altura :: Float
} deriving (Show, Eq)


tiroMuerto = CTiro 0 0 0

----LOS PALOS -----------------------------------------------------------------------------------------
type Palo = Participante -> Tiro 

putter :: Palo
putter participante = CTiro 10  (modificarHabilidad (*2) (precision participante)) 0 

madera :: Palo
madera participante = CTiro 100 (modificarHabilidad (*0.5) (precision participante)) 5  

hierro :: Float -> Palo
hierro n participante = CTiro (modificarHabilidad (*n) (fuerza participante)) (modificarHabilidad (/n) (precision participante)) (n ^ 2)

modificarHabilidad = ($)

hacerGolpe = ($)


----LOS OBSTACULOS--------------------------------------------------------------------------------------
type Obstaculo = Tiro -> Tiro

tunelConRampita :: Obstaculo
tunelConRampita tiro | atraviesaTunel tiro = tiro {velocidad = velocidad tiro * 2, precisionTiro = 100}

laguna :: Float -> Obstaculo
laguna largo tiro | atraviesaLaguna tiro = tiro {altura = altura tiro / largo}

hoyo :: Obstaculo
hoyo tiro | atraviesaHoyo tiro = tiroMuerto


type CondicionObstaculo = Tiro -> Bool

atraviesaTunel tiro = precisionTiro tiro > 80 && altura tiro == 0

atraviesaLaguna tiro = velocidad tiro > 80 && rangoEfecto 10 50 (altura tiro)

atraviesaHoyo tiro = rangoEfecto 5 20 (velocidad tiro) && precisionTiro tiro > 95 && altura tiro == 0


rangoEfecto minimo maximo efecto =  (efecto >= minimo) && (efecto <= maximo)


------------------------------------------------------------------------------------------------
--------------------------------         TEST     ----------------------------------------------
--MODELADO DE CASOS DE PRUEBA parte 1:
-- 4.a)
bart = CParticipante {nombre = "Bart", padre = "Homero", fuerza = 25.0, precision = 60.0}
todd = CParticipante {nombre = "Todd", padre = "Ned", fuerza = 15.0, precision = 80.0}
rafa = CParticipante {nombre = "Rafa", padre = "Clancy", fuerza = 10.0, precision = 1.0}



runTests1 = hspec $ do
-- 4.b)
  describe "Golpear con Palos:" $ do
    it "Bart golpea con putter" $ do
      hacerGolpe putter bart `shouldBe` (CTiro 10 120 0)
    it "Todd golpea con madera" $ do
      hacerGolpe madera todd `shouldBe` (CTiro 100 40 5)
    it "Rafa golpea con hierro N° 7" $ do
      hacerGolpe (hierro 7) rafa `shouldBe` (CTiro 70 0 49)

  describe "Sortear Obstaculos" $ do
    it "Bart atraviesa tunel con rampita" $ do
      tunelConRampita (putter bart) `shouldBe` (CTiro 20 100 0)
    it "Bart se queda en hoyo" $ do
      atraviesaHoyo (putter bart) `shouldBe` True
    it "Todd atraviesa una laguna de 2 metros" $ do
      atraviesaLaguna ((laguna 2) (hierro 7 todd)) `shouldBe` True