module WaveMachine.Audio.Voices where

import WaveMachine.Audio.Waves

sineWave :: WaveFunction
sineWave t = sin (t * 2 * pi)

harmonic :: Int -> WaveFunction
harmonic n t = sineWave (t * multiplier) / multiplier where multiplier = fromIntegral n

addHarmonic :: Int -> WaveFunction
addHarmonic n = addHarmonics [n]
    
addHarmonics :: [Int] -> WaveFunction
addHarmonics = foldr (addWaves . harmonic) sineWave
        
periodic :: Double -> Double
periodic t = t - fromInteger (floor t)
        
squareWave :: WaveFunction
squareWave t
    | t < 0.5   =  1
    | t < 1.0   = -1 
    | otherwise = squareWave $ periodic t

sawtoothWave :: WaveFunction
sawtoothWave t = (periodic t * 2) - 1
