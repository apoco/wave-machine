import Data.ByteString.Lazy.Builder
import Data.Int
import System.IO
import WaveMachine.Audio.Pitch
import WaveMachine.Audio.Sequencing
import WaveMachine.Audio.Waves
import WaveMachine.Builders
import WaveMachine.Sampling

notes :: [(Int,Tone,Int)]
notes = [
    (1,D,5), (1,E,5), 
    (2,F,5), (1,E,5), (1,D, 5), (2,Cs,5), (1,D,5), (1,E, 5),
    (2,A,4), (1,B,4), (1,Cs,5), (2,D, 5), (1,C,5), (1,Bf,4),
    (2,A,4), (1,G,4), (1,F, 4), (2,G, 4), (2,A,4),
    (1,G,4), (1,F,4), (1,E, 4), (1,F, 4), (4,D,3) ]

noteLenth :: Double
noteLenth = 0.25

playNote :: (WaveFunction,Double) -> (Int,Tone,Int) -> (WaveFunction,Double)
playNote (currentSeq,pos) (dur,tone,oct) = (
    addWaves currentSeq (delay pos $ clip (fromIntegral dur * noteLenth) $ applyTone tone oct sineWave), 
    pos + fromIntegral dur * noteLenth)

audioSeq :: [(Int,Tone,Int)] -> (WaveFunction,Double)
audioSeq = foldl playNote (nullWave, 0)

channels :: Int
channels = 1

sampleRate :: Int
sampleRate = 44100

bitDepth :: Int
bitDepth = 16

samples :: [Int16]
samples = sampleInt16 melody sampleRate dur
    where (melody,dur) = audioSeq notes

waveFile :: WaveFile Int16
waveFile = WaveFile channels sampleRate bitDepth samples

main :: IO ()
main = hPutBuilder stdout $ waveFilePcm16Builder waveFile
