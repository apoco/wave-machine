import Data.ByteString.Lazy.Builder
import Data.Int
import Data.Monoid
import System.IO
import WaveMachine.Audio.Tones
import WaveMachine.Audio.Pitch
import WaveMachine.Audio.Volume

import qualified Data.ByteString.Lazy as BS

duration = 5
samplesPerSecond = 44100.0
bytesPerSample = 2
numChannels = 1

audioFn = applyVolume 1 $ applyPitch middleC $ sineWave

timeBetweenSamples :: Double
timeBetweenSamples = 1.0 / samplesPerSecond

allSampleTimes :: [Double]
allSampleTimes = sampleTimes 0

sampleTimes :: Double -> [Double]
sampleTimes t
    | t > duration = []
    | otherwise = t:(sampleTimes (t + timeBetweenSamples))

sampleFor :: Double -> Double
sampleFor time = audioFn time

sampleValues :: [Double]
sampleValues = map sampleFor allSampleTimes

intForSample :: Double -> Int16
intForSample v = floor (v * 65535 / 2.0)

sampleInts :: [Int16] 
sampleInts = map intForSample sampleValues

generateAudioData :: Builder
generateAudioData = mconcat (map int16LE sampleInts)
  
main :: IO ()
main = hPutBuilder stdout generateAudioData
