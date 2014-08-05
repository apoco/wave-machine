module WaveMachine.Audio.Volume where

import WaveMachine.Audio.Waves

applyVolume :: Double -> WaveFunction -> WaveFunction
applyVolume volume orig = \t -> (orig t) * volume
