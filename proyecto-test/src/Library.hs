module Library where
import PdePreludat

data Persona = Persona {
    nombre :: String,
    nivelSatisfaccion :: Number,
    nivelEmocion :: Number,
    nivelCultura :: Number
} deriving Show

ana = Persona "Ana" 10 20 60
juan = Persona "Juan" 20 30 40

type Atraccion = Persona -> Persona

montañaRusa :: Number -> Number -> Atraccion
montañaRusa velocidad altura persona
    | velocidad > 50 = (aumentarEmocion (0.15*velocidad + altura) persona)
    | otherwise = ((disminuirEmocion 5) . (disminuirSatisfaccion 10)) persona

aumentarEmocion cantidad persona  = persona {nivelEmocion = (nivelEmocion persona) + cantidad}

disminuirEmocion porcentaje persona = persona {nivelEmocion = (nivelEmocion persona) * (100 - porcentaje)/100}

disminuirSatisfaccion porcentaje persona = persona {nivelSatisfaccion = (nivelSatisfaccion persona)* (100-porcentaje)/100}

caidaLibre :: Number -> Atraccion
caidaLibre metrosCaida persona = aumentarEmocion (metrosCaida * 0.2) persona

mundoMaya :: Atraccion
mundoMaya persona = ((aumentarEmocion ((nivelEmocion persona) * 0.1 )) . (aumentarCultura ((nivelCultura persona) * 0.2))) persona

aumentarCultura cantidad persona = persona {nivelCultura = (nivelCultura persona) + cantidad}

showDeMagia :: Atraccion
showDeMagia persona
    | (nivelCultura persona) > 50 = (aumentarSatisfaccion 20 persona)
    | otherwise = (aumentarEmocion 30 persona)

aumentarSatisfaccion cantidad persona = persona { nivelSatisfaccion = (nivelSatisfaccion persona) + cantidad}

saltoBungee :: Atraccion
saltoBungee persona = (caidaLibre 20) . (montañaRusa 200 10) $ persona

zombieWalk :: Atraccion
zombieWalk persona = showDeMagia . (montañaRusa 100 50) $ persona


