module WaveMachine.Audio.Tones where

sineWave :: Double -> Double
sineWave t = sin (t * 2 * pi)
