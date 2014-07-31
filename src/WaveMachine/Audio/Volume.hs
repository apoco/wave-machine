module WaveMachine.Audio.Volume where

applyVolume :: Double -> (Double -> Double) -> (Double -> Double)
applyVolume volume orig = \t -> (orig t) * volume
