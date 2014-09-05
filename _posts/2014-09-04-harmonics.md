---
title: Harmonics
layout: post
---

In the previous article, I made mention of _harmonics_. _Harmonics_ are components of a wave form that have a frequency that is a multiple of the
_fundamental_ frequency. For most people, harmonics are not perceived as separate tones, but rather part of the main tone. This is because the
harmonic _also_ repeats at the fundamental frequency. Many real-world instruments produce harmonics, so they are an important concept in music.

From now on, I'm going to call wave functions used for musical tones _voices_. In `Main.hs`, I'll make the voice a function so we can easily
swap it with other `WaveFunction` values:

```haskell
voice :: WaveFunction
voice = sineWave

-- ...

playNote :: (WaveFunction,Double) -> (Int,Tone,Int) -> (WaveFunction,Double)
playNote (currentSeq,pos) (dur,tone,oct) = 
    (addWaves currentSeq thisNote, pos + duration)
    where 
        duration = fromIntegral dur * noteLength
        thisNote = delay pos $ clip duration $ applyTone tone oct voice
```

I'll move `sineWave` to `Voices.hs`, where I can add additional voices later on:

```haskell
module WaveMachine.Audio.Voices where

import WaveMachine.Audio.Waves

sineWave :: WaveFunction
sineWave t = sin (t * 2 * pi)
```

Let's create a function that will add a harmonic sine wave to another:

```haskell
harmonic :: Int -> WaveFunction
harmonic n t = sineWave (t * multiplier) / multiplier where multiplier = fromIntegral n

addHarmonic :: Int -> WaveFunction
addHarmonic n = addHarmonics [n]
    
addHarmonics :: [Int] -> WaveFunction
addHarmonics = foldr (addWaves . harmonic) sineWave
```

The `harmonic` function takes an integer and returns a `sineWave` that is the frequency times that integer. I've also reduced the volume of this
harmonic so it doesn't overwhelm the main component of the wave. The `addHarmonic` function simply calls `addHarmonics` with just one harmonic
value, and `addHarmonics` actually combines the waves.

The `addHarmonics` function has a few new things I should point out to fellow Haskell beginners. This `foldr` function isn't so different than the
`foldl` function we used earlier; it just means "fold right" instead of "fold left," so it starts combining values starting with the right side
of a list. This `addWaves . harmonic` is a bit more advanced. The `.` operator is used for _function composition_, and is meant to resemble the
`⸰` mathematical operator. `(f . g) x` is the same as `f (g x)`. `addWaves . harmonic` produces a function that will take an integer harmonic level
and create a function that takes a `WaveFunction` and combines it with the harmonic.

So let's hear what it sounds like when we do this:

```haskell
voice = addHarmonic 2
```

<audio src="{{site.url}}/audio/harmonic2.wav" controls/>

The harmonic in this case is an octave higher, since doubling a frequency produces an octave interval. So what does a harmonic of 3 do?

```haskell
voice = addHarmonic 3
```

<audio src="{{site.url}}/audio/harmonic3.wav" controls/>

Recall that each octave is 12 tones. Multiplying the frequency by 2 causes us to go up all 12 tones, since `2 = 2 ^ (12/12)`; multiplying by three
does not lead to an exact tone in the 12-tone scale, but the closest value is an octave + "perfect fifth" (19 tones). A harmonic of four is two
octaves (24 tones):

```haskell
voice = addHarmonic 4
```

<audio src="{{site.url}}/audio/harmonic4.wav" controls/>

Five is also not an even tone, but most closely matches two octaves + a major third (28), and six most closely resembles two octaves plus a
fifth (31):

```haskell
voice = addHarmonic 5
```

<audio src="{{site.url}}/audio/harmonic5.wav" controls/>

```haskell
voice = addHarmonic 6
```

<audio src="{{site.url}}/audio/harmonic6.wav" controls/>

Here is how all 6 harmonics together in one wave sounds:

```haskell
voice = addHarmonics [2..6]
```

<audio src="{{site.url}}/audio/1-6.wav" controls/>

And just for fun, here are some waves with just odds and just octave harmonics. 

```haskell
voice = addHarmonics [2, 4, 8, 16]
```

<audio src="{{site.url}}/audio/evens.wav" controls/>

```haskell
voice = addHarmonics [3, 5, 7, 9]
```

<audio src="{{site.url}}/audio/odds.wav" controls/>

Sine waves + harmonics are not the only choices we have for voices. One common synthesized sound is a square wave, where the oscillating pattern
just jumps between two values:

```haskell
periodic :: Double -> Double
periodic t = t - fromInteger (floor t)
        
squareWave :: WaveFunction
squareWave t
    | t < 0.5   =  1
    | t < 1.0   = -1 
    | otherwise = squareWave $ periodic t
```

voice = squareWave
```

<audio src="{{site.url}}/audio/square.wav" controls/>

Sawtooth waves on the other hand are a triangular, jagged wave shape:

```haskell
sawtoothWave :: WaveFunction
sawtoothWave t = (periodic t * 2) - 1

voice = sawtoothWave
```

<audio src="{{site.url}}/audio/sawtooth.wav" controls/>

It's fun to play with all these synthetic voices, but in the next article I'm going to experiment with trying to make a voice
that more closely resembles a real musical instrument.
