module WaveMachine.Sampling where

import Data.Int
import WaveMachine.Audio.Waves

sampleTimes :: Int -> Double -> Double -> [Double]
sampleTimes frequency start end
    | start > end = []
    | otherwise = start : sampleTimes frequency (start + (1.0 / fromIntegral frequency)) end

sample :: WaveFunction -> Int -> Double -> [Double]
sample audioFn frequency duration = [audioFn t | t <- sampleTimes frequency 0 duration]

sampleInt16 :: WaveFunction -> Int -> Double -> [Int16]
sampleInt16 audioFn frequency duration = 
    [floor (v * 32767.5 / maxMagnitude) | v <- samples]
    where 
        samples = sample audioFn frequency duration
        maxMagnitude = maximum (map abs samples)
