module Library where
import PdePreludat


--Punto 1

data Persona = Persona {
    nombre :: String,
    nivelSatisfaccion :: Number,
    nivelEmocion :: Number,
    nivelCultura :: Number
} deriving Show

ana = Persona "Ana" 10 20 60
juan = Persona "Juan" 20 30 40
{----------------------------------------------------------------}


{-
    Funciones auxiliares
-}

aumentarEmocion :: Number -> Persona -> Persona
aumentarEmocion cantidad persona  = persona {nivelEmocion = (nivelEmocion persona) + cantidad}

disminuirEmocion :: Number -> Persona -> Persona
disminuirEmocion porcentaje persona = persona {nivelEmocion = (nivelEmocion persona) * (100 - porcentaje)/100}

disminuirSatisfaccion :: Number -> Persona -> Persona
disminuirSatisfaccion porcentaje persona = persona {nivelSatisfaccion = (nivelSatisfaccion persona)* (100-porcentaje)/100}

aumentarCultura :: Number -> Persona -> Persona
aumentarCultura cantidad persona = persona {nivelCultura = (nivelCultura persona) + cantidad}

aumentarPorcentajeEmocion :: Number -> Persona -> Persona
aumentarPorcentajeEmocion porcentaje persona = aumentarEmocion ((nivelEmocion persona)*porcentaje) persona

aumentarPorcentajeCultura :: Number -> Persona -> Persona
aumentarPorcentajeCultura porcentaje persona = aumentarCultura ((nivelCultura persona)*porcentaje) persona

aumentarSatisfaccion :: Number -> Persona -> Persona
aumentarSatisfaccion cantidad persona = persona { nivelSatisfaccion = (nivelSatisfaccion persona) + cantidad}


--Punto 2
type Atraccion = Persona -> Persona

montañaRusa :: Number -> Number -> Atraccion
montañaRusa velocidad altura persona
    | velocidad > 50 = aumentarEmocion (0.15*velocidad + altura) persona
    | otherwise = ((disminuirEmocion 5) . (disminuirSatisfaccion 10)) persona

caidaLibre :: Number -> Atraccion
caidaLibre metrosCaida persona = aumentarEmocion (metrosCaida * 0.2) persona

mundoMaya :: Atraccion
mundoMaya persona = ((aumentarPorcentajeEmocion 0.1) . (aumentarPorcentajeCultura 0.2)) persona

showDeMagia :: Atraccion
showDeMagia persona
    | (nivelCultura persona) > 50 = aumentarSatisfaccion 20 persona
    | otherwise = aumentarEmocion 30 persona

saltoBungee :: Atraccion
saltoBungee persona = (caidaLibre 20) . (montañaRusa 200 10) $ persona

zombieWalk :: Atraccion
zombieWalk persona = showDeMagia . (montañaRusa 100 50) $ persona


