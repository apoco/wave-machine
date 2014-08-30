module WaveMachine.Audio.Sequencing where

import WaveMachine.Audio.Waves

delay :: Double -> WaveFunction -> WaveFunction
delay delayTime waveFunc t = if t < delayTime then 0 else waveFunc (t - delayTime)

clip :: Double -> WaveFunction -> WaveFunction
clip duration waveFunc t = if t > duration then 0 else waveFunc t
