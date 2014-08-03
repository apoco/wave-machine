module WaveMachine.Builders where

import Data.ByteString.Lazy.Builder
import Data.Int
import Data.Monoid
import Data.Word

import qualified Data.ByteString.Lazy as Lazy


data WaveFile a = WaveFile Int Int Int [a] -- channels, sampleRate, bitDepth, samples

data RiffFile = RiffFile [RiffChunk]

data RiffHeader = RiffHeader String Int64

data RiffChunk =
    RiffChunk RiffHeader Lazy.ByteString
    | RiffFormChunk String [RiffChunk]
    | WaveFormatChunk WaveFormat
    | WaveInt16SamplesChunk [Int16]
    
data WaveFormat = 
    WaveFormat Int Int Int Int Lazy.ByteString -- format, channels, sampleRate, bitDepth, extra
    | PcmFormat Int Int Int                    -- channels, sampleRate, bitDepth
    

sizeOfRiffChunks :: [RiffChunk] -> Int64
sizeOfRiffChunks chunks = sum $ map sizeOfRiffChunk chunks


sizeOfRiffChunk :: RiffChunk -> Int64
sizeOfRiffChunk (RiffChunk header bytes) = sizeOfRiffHeader + (Lazy.length bytes)
sizeOfRiffChunk (RiffFormChunk form chunks) = sizeOfRiffHeader + 4 + (sizeOfRiffChunks chunks) 
sizeOfRiffChunk (WaveFormatChunk format) = sizeOfRiffHeader + (sizeOfWaveFormat format)
sizeOfRiffChunk (WaveInt16SamplesChunk samples) = sizeOfRiffHeader + (fromIntegral (2 * (length samples)))


sizeOfRiffHeader :: Int64
sizeOfRiffHeader = 8


sizeOfWaveFormat :: WaveFormat -> Int64
sizeOfWaveFormat (WaveFormat _ _ _ _ params) = 18 + (Lazy.length params)
sizeOfWaveFormat (PcmFormat _ _ _) = 18


riffFileBuilder :: RiffFile -> Builder
riffFileBuilder (RiffFile chunks) = mconcat (map riffChunkBuilder chunks)


riffHeaderBuilder :: RiffHeader -> Builder
riffHeaderBuilder (RiffHeader tag size) = mconcat [ 
    string8 tag, 
    word32LE $ fromIntegral size ]


riffChunkBuilder :: RiffChunk -> Builder
riffChunkBuilder (RiffChunk header bytes) = mconcat [ 
    riffHeaderBuilder header, 
    lazyByteString bytes ]
riffChunkBuilder (RiffFormChunk form chunks) = mconcat ( 
    [
        riffHeaderBuilder (RiffHeader "RIFF" (4 + (sizeOfRiffChunks chunks))),
        string8 "WAVE"
    ] ++ (map riffChunkBuilder chunks))
riffChunkBuilder (WaveFormatChunk waveFormat) = mconcat [ 
    riffHeaderBuilder (RiffHeader "fmt " (sizeOfWaveFormat waveFormat)), 
    waveFormatBuilder waveFormat ]
riffChunkBuilder (WaveInt16SamplesChunk samples) = mconcat (
    riffHeaderBuilder (RiffHeader "data" (2 * (fromIntegral $ length samples)))
    : (map int16LE samples))


waveFormatBuilder :: WaveFormat -> Builder
waveFormatBuilder (WaveFormat format channels sampleRate bitDepth extra) = 
    mconcat [
        word16LE $ fromIntegral format,                             -- 1 = PCM
        word16LE $ fromIntegral channels,                           -- Channels
        word32LE $ fromIntegral sampleRate,                         -- Sample rate (Hz)
        word32LE $ fromIntegral $ sampleRate * channelsSampleSize,  -- Byte rate (all channels)
        word16LE $ fromIntegral channelsSampleSize,                 -- Bytes per sample (all channels)
        word16LE $ fromIntegral bitDepth,                           -- Bits per sample
        word16LE $ fromIntegral $ Lazy.length extra,                -- Size of extra data
        lazyByteString extra]                                                
    where channelsSampleSize = channels * (bitDepth `div` 8)
waveFormatBuilder (PcmFormat channels sampleRate bitDepth) = 
    waveFormatBuilder (WaveFormat 1 channels sampleRate bitDepth Lazy.empty)


audioBuilder :: [Int16] -> Builder
audioBuilder samples = mconcat sampleBuilders
    where sampleBuilders = map int16LE samples

  
waveFileBuilder :: WaveFile Int16 -> Builder
waveFileBuilder (WaveFile channels sampleRate bitDepth samples) = riffFileBuilder (
    RiffFile [ 
        RiffFormChunk "WAVE" [ 
            WaveFormatChunk ( PcmFormat channels sampleRate bitDepth ),
            WaveInt16SamplesChunk samples ] ])
            