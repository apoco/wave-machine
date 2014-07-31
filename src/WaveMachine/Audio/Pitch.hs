module WaveMachine.Audio.Pitch where

middleC = 261.63

applyPitch :: Double -> (Double -> Double) -> (Double -> Double)
applyPitch pitch orig = \t -> orig (t * pitch)
