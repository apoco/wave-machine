module WaveMachine.Sampling where

import Data.Int

sampleTimes :: Double -> Double -> Double -> [Double]
sampleTimes frequency start end
    | start > end = []
    | otherwise = start:(sampleTimes frequency (start + (1.0 / frequency)) end)

sample :: (Double -> Double) -> Double -> Double -> [Double]
sample audioFn frequency duration = [audioFn t | t <- sampleTimes frequency 0 duration]

sampleInt16 :: (Double -> Double) -> Double -> Double -> [Int16]
sampleInt16 audioFn frequency duration = 
    [floor (v * 32767.5) | v <- samples]
    where samples = sample audioFn frequency duration
