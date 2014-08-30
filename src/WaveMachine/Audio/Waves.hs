module WaveMachine.Audio.Waves where

type WaveFunction = (Double -> Double)

nullWave :: WaveFunction
nullWave _ = 0

sineWave :: WaveFunction
sineWave t = sin (t * 2 * pi)

addWaves :: WaveFunction -> WaveFunction -> WaveFunction
addWaves a b t = a t + b t

combineWaves :: [WaveFunction] -> WaveFunction
combineWaves = foldl addWaves nullWave
