---
title: Sequencing
layout: post
---

Now that we added automatic amplitude adjustment in the previous article, it will be much easier to combine waves however I see fit. I'm eager to
start producing melodies, so now I'm going to do some _sequencing_. By sequencing, I mean simply creating a sequence of _notes_. A _note_ is a
combination of a _duration_ and a _tone_. We've already figured out how to produce tones; to give a note a particular duration, I'm going to create
a `clip` function in the new `Sequencing.hs` module:

```haskell
clip :: Double -> WaveFunction -> WaveFunction
clip duration waveFunc t = if t > duration then 0 else waveFunc t
```

The `clip` function is simply a _step_ function, where the original sound function is played for `duration` seconds, after which the sound
ceases (a `0` value is produced). To support sequencing, we'll also need to do the opposite, and delay a `WaveFunction` for a number of seconds:

```haskell
delay :: Double -> WaveFunction -> WaveFunction
delay delayTime waveFunc t = if t < delayTime then 0 else waveFunc (t - delayTime)
```

The `delay` function basically outputs `0` for `delayTime` seconds, and shifts the `WaveFunction` in time by that same amount.

Now let's put this all together in `Main.hs`. I want the code for writing my melody to feel really natural, so I'm going with this type signature:

```haskell
notes :: [(Int,Tone,Int)]
```

This `notes` function will be a `List` of `(Int,Tone,Int)`. This is the first time we're using a Haskell `Tuple`, which is simply an aggregate data
type with positional values. In this case, there will be an `Int` representing a duration, `Tone` representing the tone, of course, and an `Int`
for the _octave_. Here are my notes:

```haskell
notes = [
    (1,D,5), (1,E,5), 
    (2,F,5), (1,E,5), (1,D, 5), (2,Cs,5), (1,D,5), (1,E, 5),
    (2,A,4), (1,B,4), (1,Cs,5), (2,D, 5), (1,C,5), (1,Bf,4),
    (2,A,4), (1,G,4), (1,F, 4), (2,G, 4), (2,A,4),
    (1,G,4), (1,F,4), (1,E, 4), (1,F, 4), (4,D,4) ]
```

The duration value is an integer for the sake of simplicity; I'll be treating each unit like an eighth note. Each eighth note will be a quarter
of a second:re

```haskell
noteLenth :: Double
noteLenth = 0.25
```

So how do we go from a sequence of notes to a `WaveFunction`? As we saw previously, combining multiple tones into a _chord_ was simply adding the
function values together. Sequencing is the same, only we're delaying and clipping the notes before combining. We also need to keep track of how
how far into the sequence we are going by maintaining an _accumulator_ for the total number of seconds:

```haskell
audioSeq :: [(Int,Tone,Int)] -> (WaveFunction,Double)
audioSeq = foldl playNote (nullWave, 0)
```

The `foldl` function here means _fold left_. The `foldl` function starts with an initial value and combines it with the first
value in the list using a given function. This resulting value is then combined with the next value in the list with the same function. This repeats
until all values are "folded" into one result value. In our case, the initial value is `(nullWave, 0)`; we start with silence (`nullWave`), with our
time accumulator starting at zero. The list we'll be folding is the sequence of notes, and the function to combine them is `playNote`
which is defined as follows:

```haskell
playNote :: (WaveFunction,Double) -> (Int,Tone,Int) -> (WaveFunction,Double)
playNote (currentSeq,pos) (dur,tone,oct) = 
    (addWaves currentSeq thisNote, pos + duration)
    where 
        duration = fromIntegral dur * noteLength
        thisNote = delay pos $ clip duration $ applyTone tone oct sineWave
```

We simply create a `sineWave` tone at a given pitch and octave, clip it to its given duration, and delay it so it's at the end of the sequence.
This tone is added to the sequence produced so far, and the time accumulator is advanced by the duration of the note. Now let's produce the audio
samples from this audio sequence:

```haskell
samples :: [Int16]
samples = sampleInt16 melody sampleRate dur
    where (melody,dur) = audioSeq notes
```

And now we some something resembling music!

To see the code in its entirety, check out the [article-5 branch](https://github.com/apoco/wave-machine/tree/article-5) of the `wave-machine`
repository.

I'm excited that I was able to create a simple melody. My goal in the next article will be to combine this melody with a base line.
