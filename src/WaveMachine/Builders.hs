module WaveMachine.Builders where

import Data.ByteString.Lazy.Builder
import Data.Int
import Data.Monoid
import Data.Word


data WaveFile a = WaveFile Int Int Int [a] -- channels, sampleRate, bitDepth, samples

data RiffFile = RiffFile [RiffChunk]

data RiffHeader = RiffHeader String Int

data RiffChunk =
    RiffChunk RiffHeader [Word8]
    | RiffFormChunk String [RiffChunk]
    | WaveFormatChunk Int Int Int Int [Word8] -- format, channels, sampleRate, bitDepth, extra
    | WaveInt16SamplesChunk [Int16]


pcmFormat :: Int
pcmFormat = 1    

riffFileForWavePcm16 :: WaveFile Int16 -> RiffFile
riffFileForWavePcm16 (WaveFile channels sampleRate bitDepth samples) =
    RiffFile [ 
        RiffFormChunk "WAVE" [ 
            WaveFormatChunk pcmFormat channels sampleRate bitDepth [],
            WaveInt16SamplesChunk samples ] ]


waveFilePcm16Builder :: WaveFile Int16 -> Builder
waveFilePcm16Builder waveFile = riffFileBuilder $ riffFileForWavePcm16 waveFile 
            

riffFileBuilder :: RiffFile -> Builder
riffFileBuilder (RiffFile chunks) = mconcat $ map riffChunkBuilder chunks


riffChunkBuilder :: RiffChunk -> Builder

riffChunkBuilder (RiffChunk header bytes) = mconcat ( 
    riffHeaderBuilder header : map word8 bytes)

riffChunkBuilder (RiffFormChunk form chunks) = mconcat ( 
    [
        riffHeaderBuilder (RiffHeader "RIFF" (4 + sizeOfRiffChunks chunks)),
        string8 form
    ]
    ++ map riffChunkBuilder chunks)

riffChunkBuilder (WaveFormatChunk format channels sampleRate bitDepth extraBytes) = 
    mconcat ([
        riffHeaderBuilder $ RiffHeader "fmt " (fromIntegral $ 18 + length extraBytes),
        word16LE $ fromIntegral format, word16LE $ fromIntegral channels,
        word32LE $ fromIntegral sampleRate,
        word32LE $ fromIntegral $ sampleRate * channelsSampleSize,
        word16LE $ fromIntegral channelsSampleSize,
        word16LE $ fromIntegral bitDepth,
        word16LE $ fromIntegral $ length extraBytes
    ] ++ map word8 extraBytes)
    where channelsSampleSize = channels * (bitDepth `div` 8)

riffChunkBuilder (WaveInt16SamplesChunk samples) = mconcat (
    riffHeaderBuilder (RiffHeader "data" (2 * fromIntegral (length samples)))
    : map int16LE samples)


riffHeaderBuilder :: RiffHeader -> Builder
riffHeaderBuilder (RiffHeader tag size) = mconcat [ 
    string8 tag, 
    word32LE $ fromIntegral size ]

sizeOfRiffChunks :: [RiffChunk] -> Int
sizeOfRiffChunks chunks = sum $ map sizeOfRiffChunk chunks

sizeOfRiffChunk :: RiffChunk -> Int
sizeOfRiffChunk chunk = sizeOfRiffHeader + sizeOfRiffContent chunk

sizeOfRiffHeader :: Int
sizeOfRiffHeader = 8

sizeOfRiffContent :: RiffChunk -> Int
sizeOfRiffContent (RiffChunk _ bytes)             = length bytes
sizeOfRiffContent (RiffFormChunk _ chunks)        = 4 + sizeOfRiffChunks chunks 
sizeOfRiffContent (WaveFormatChunk _ _ _ _ extra) = 18 + length extra
sizeOfRiffContent (WaveInt16SamplesChunk samples) = fromIntegral $ 2 * length samples
