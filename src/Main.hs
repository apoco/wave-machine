import Data.ByteString.Lazy.Builder
import System.IO
import WaveMachine.Audio.Tones
import WaveMachine.Audio.Pitch
import WaveMachine.Builders
import WaveMachine.Sampling

audioFn :: Double -> Double
audioFn = applyPitch middleC sineWave

channels = 1
sampleRate = 44100
bitDepth = 16
samples = sampleInt16 audioFn sampleRate 5
waveFile = WaveFile channels sampleRate bitDepth samples

main :: IO ()
main = hPutBuilder stdout $ waveFilePcm16Builder waveFile
