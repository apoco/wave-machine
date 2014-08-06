import Data.ByteString.Lazy.Builder
import Data.Monoid
import System.IO
import WaveMachine.Audio.Pitch
import WaveMachine.Audio.Volume
import WaveMachine.Audio.Waves
import WaveMachine.Builders
import WaveMachine.Sampling

audioFn :: Double -> Double
audioFn = addWaves [
    applyPitch (toneFrequency C 4) sineWave,
    applyPitch (toneFrequency E 4) sineWave,
    applyPitch (toneFrequency G 4) sineWave ]

channels = 1
sampleRate = 44100
bitDepth = 16
samples = sampleInt16 audioFn sampleRate 5
waveFile = WaveFile channels sampleRate bitDepth samples

main :: IO ()
main = hPutBuilder stdout $ waveFilePcm16Builder waveFile
