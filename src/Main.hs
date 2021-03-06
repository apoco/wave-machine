import Data.ByteString.Lazy.Builder
import Data.Int
import System.IO
import WaveMachine.Audio.Pitch
import WaveMachine.Audio.Sequencing
import WaveMachine.Audio.Voices
import WaveMachine.Audio.Waves
import WaveMachine.Builders
import WaveMachine.Sampling

voice :: WaveFunction
--voice = sineWave
--voice = addHarmonic 2
--voice = addHarmonic 3
--voice = addHarmonic 4
--voice = addHarmonic 5
--voice = addHarmonic 6
--voice = addHarmonics [2..6]
--voice = addHarmonics [3, 5, 7, 9]
--voice = addHarmonics [2, 4, 8, 16]
--voice = squareWave
voice = sawtoothWave

rhNotes :: [(Int,Tone,Int)]
rhNotes = [
    (1,D,5), (1,E,5), 
    (2,F,5), (1,E,5), (1,D, 5), (2,Cs,5), (1,D,5), (1,E, 5),
    (2,A,4), (1,B,4), (1,Cs,5), (2,D, 5), (1,C,5), (1,Bf,4),
    (2,A,4), (1,G,4), (1,F, 4), (2,G, 4), (2,A,4),
    (1,G,4), (1,F,4), (1,E, 4), (1,F, 4), (4,D,4) ]

lhNotes :: [(Int,Tone,Int)]
lhNotes = [
    (1,F,3), (1,E,3), 
    (2,D,3), (2,F,3), (2,A, 3), (2,G, 3),
    (2,F,3), (2,E,3), (2,D ,3), (2,C, 3),
    (2,F,3), (2,A,3), (2,G, 3), (2,Cs,3),
    (8,D,3)]

noteLength :: Double
noteLength = 0.25

audioSeq :: [(Int,Tone,Int)] -> (WaveFunction,Double)
audioSeq = foldl playNote (nullWave, 0)

playNote :: (WaveFunction,Double) -> (Int,Tone,Int) -> (WaveFunction,Double)
playNote (currentSeq,pos) (dur,tone,oct) = 
    (addWaves currentSeq thisNote, pos + duration)
    where 
        duration = fromIntegral dur * noteLength
        thisNote = delay pos $ clip duration $ applyTone tone oct voice

channels :: Int
channels = 1

sampleRate :: Int
sampleRate = 44100

bitDepth :: Int
bitDepth = 16

samples :: [Int16]
samples = sampleInt16 (addWaves rhAudio lhAudio) sampleRate dur
    where 
        (rhAudio,dur) = audioSeq rhNotes
        (lhAudio,_) = audioSeq lhNotes

waveFile :: WaveFile Int16
waveFile = WaveFile channels sampleRate bitDepth samples

main :: IO ()
main = hPutBuilder stdout $ waveFilePcm16Builder waveFile
