module WaveMachine.Audio.Pitch where

import WaveMachine.Audio.Waves

data Tone = A | As | Bf | B | C | Cs | Df | D | Ds | Ef | E | F | Fs | Gf | G | Gs | Af

octaveIndex :: Tone -> Int
octaveIndex C  =  0
octaveIndex Cs =  1
octaveIndex Df =  1
octaveIndex D  =  2
octaveIndex Ds =  3
octaveIndex Ef =  3
octaveIndex E  =  4
octaveIndex F  =  5
octaveIndex Fs =  6
octaveIndex Gf =  6
octaveIndex G  =  7
octaveIndex Gs =  8
octaveIndex Af =  8
octaveIndex A  =  9
octaveIndex As = 10
octaveIndex Bf = 10
octaveIndex B  = 11

toneIndex :: Tone -> Int -> Int
toneIndex tone octave = 12 * (octave - 4) + (octaveIndex tone - 9) 

a4 :: Double
a4 = 440

toneFrequency :: Tone -> Int -> Double
toneFrequency tone octave = a4 * (2.0 ** (fromIntegral (toneIndex tone octave) / 12.0))

applyTone :: Tone -> Int -> WaveFunction -> WaveFunction
applyTone tone oct = applyPitch (toneFrequency tone oct)

applyPitch :: Double -> WaveFunction -> WaveFunction
applyPitch pitch orig t = orig (t * pitch)

middleC :: Double
middleC = toneFrequency C 4
