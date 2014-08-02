import Data.ByteString.Lazy.Builder
import Data.Monoid
import System.IO
import WaveMachine.Audio.Tones
import WaveMachine.Audio.Pitch
import WaveMachine.Sampling

audioFn :: Double -> Double
audioFn = applyPitch middleC sineWave

samples = sampleInt16 audioFn 44100.0 5

audioBuilder :: Builder
audioBuilder = mconcat sampleBuilders
    where sampleBuilders = map int16LE samples
  
main :: IO ()
main = hPutBuilder stdout audioBuilder
