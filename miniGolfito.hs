import Text.Show.Functions
import Test.Hspec
import Data.List

------------------------------------------------------------------------------------------------------
-- MINIGOLFITO

data Jugador = CJugador {
  nombre :: String, 
  padre :: String,
  fuerza :: Float,
  precisionJ :: Float
} deriving (Show, Eq)

data Tiro = CTiro {
  velocidad :: Float,
  precisionT :: Float,
  altura :: Float
} deriving (Show, Eq)

type Palo = Jugador -> Tiro 


----LOS PALOS -----------------------------------------------------------------------------------------
putter :: Palo
putter jugador = generarTiro (10, (modificarHabilidad (*2) (precisionJ jugador)), 0) 

madera :: Palo
madera jugador = generarTiro (100, (modificarHabilidad (/2) (precisionJ jugador)), 5)  

hierro :: Float -> Palo
hierro numero jugador = generarTiro ((fuerza jugador * numero), (modificarHabilidad (/2) (precisionJ jugador)), (numero ^ 2)) 

modificarHabilidad = ($) 

generarTiro (velocidad, precisionT, altura) = CTiro velocidad precisionT altura 

golpear = ($)


----LOS OBSTACULOS--------------------------------------------------------------------------------------
type Obstaculo = Tiro -> (Bool, Tiro)

tunelConRampita :: Obstaculo
tunelConRampita tiro = (precisionT tiro > 80 && altura tiro == 0, tiro {velocidad = velocidad tiro * 2, precisionT = 100})

laguna :: Float -> Obstaculo
laguna largo tiro = (velocidad tiro > 80 && rangoEfecto 10 50 (altura tiro), tiro {altura = altura tiro / largo})

hoyo :: Obstaculo
hoyo tiro = (rangoEfecto 5 20 (velocidad tiro) && precisionT tiro > 95 && altura tiro == 0, tiroMuerto)

superaObtaculo = fst

modificaTiroObstaculo = snd

rangoEfecto minimo maximo efecto =  (efecto >= minimo) && (efecto <= maximo)

tiroMuerto = generarTiro (0,0,0)

------------------------------------------------------------------------------------------------
--------------------------------         TEST     ----------------------------------------------
--MODELADO DE CASOS DE PRUEBA parte 1:
-- 4.a)
bart = CJugador {nombre = "Bart", padre = "Homero", fuerza = 25.0, precisionJ = 60.0}
todd = CJugador {nombre = "Todd", padre = "Ned", fuerza = 15.0, precisionJ = 80.0}
rafa = CJugador {nombre = "Rafa", padre = "Clancy", fuerza = 10.0, precisionJ = 1.0}



runTests1 = hspec $ do
-- 4.b)
  describe "Golpear con Palos:" $ do
    it "Bart golpea con putter" $ do
      golpear putter bart `shouldBe` (CTiro 10 120 0)
    it "Todd golpea con madera" $ do
      golpear madera todd `shouldBe` (CTiro 100 40 5)
    it "Rafa golpea con hierro N° 7" $ do
      golpear (hierro 7) rafa `shouldBe` (CTiro 70 0.5 49)

  describe "Sortear Obstaculos" $ do
    it "Bart atraviesa tunel con rampita" $ do
      superaObtaculo (tunelConRampita (putter bart)) `shouldBe` True
      modificaTiroObstaculo (tunelConRampita (putter bart)) `shouldBe` (CTiro 20 100 0)
    it "Bart se queda en hoyo" $ do
      superaObtaculo (hoyo (putter bart)) `shouldBe` False
    it "Todd atraviesa una laguna de 2 metros" $ do
      superaObtaculo ((laguna 2) (hierro 7 todd)) `shouldBe` True