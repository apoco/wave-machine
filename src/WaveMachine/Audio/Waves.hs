module WaveMachine.Audio.Waves where

type WaveFunction = (Double -> Double)

nullWave :: WaveFunction
nullWave _ = 0

sineWave :: WaveFunction
sineWave t = sin (t * 2 * pi)

addWaves :: [WaveFunction] -> WaveFunction
addWaves [] = nullWave
addWaves (w:[]) = w
addWaves (w:ws) = \t -> (w t) + (addWaves ws $ t)
