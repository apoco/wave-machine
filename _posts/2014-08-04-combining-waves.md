---
title: Combining Sound Waves
layout: post
---

Now that we enabled writing a `.wav` file directly in the previous article, we can start to experiment much quicker. I don't know about you, but
I find hearing just a single tone rather boring; I want to hear multiple things at the same time!

Doing this turns out to be really easy... sort of. I've learned that when two things produce sound in the real world, the pressures waves they
emit just merge, meaning the pressures amounts are just added together! I found this rather surprising, because it seems like when there are
multiple things making sound, we hear them both distinctly. In reality, we're still only hearing one stream of sound pressures (well, technically
two since we have two ears), and our brain just interpolates the incoming signals as distinct sounds. The brain is quite amazing, eh?

So I guess this makes our code pretty easy. Here's `Waves.hs` (I renamed the poorly-named `Tones.hs` because of the incorrect use of that term):

```haskell
module WaveMachine.Audio.Waves where

type WaveFunction = (Double -> Double)

nullWave :: WaveFunction
nullWave _ = 0

sineWave :: WaveFunction
sineWave t = sin (t * 2 * pi)

addWaves :: [WaveFunction] -> WaveFunction
addWaves [] = nullWave
addWaves (w:[]) = w
addWaves (w:ws) = \t -> (w t) + (addWaves ws $ t)
```

To save some typing, I've defined a *type alias* called `WaveFunction` that simply represents `Double -> Double` since we're using this sort of
function all over the place. Type aliases are convenient, but don't get too excited; what you can do with them is restricted. I learned this the
hard way when I tried something advanced, making `WaveFunction` an instance of `Monoid`. Oh well, at least we can save some keystrokes.

This new `addWaves` function takes a list of `WaveFunction`s and merges them into one `WaveFunction` (see why I wanted a `Monoid`?). You'll notice
that when it receives an empty list, it returns some weird `nullWave` function; for the sake of completeness, I created `nullWave` as the "identity"
wave function, meaning when it combines with other waves, it has no effect. If, for some reason, we get an empty list of wave functions, you'll
get this identity. Otherwise, if there's only one function, we'll return that function. Else, we combine recursively by just adding results from
the functions.

An astute reader may notice a problem. Remember before when I said that `1.0` and `-1.0` were going to be our maximum and minimum wave function
values? If we start to add waves together, they have the ability to break through these bounds. This is inconvenient because our sampling code makes
the assumption that the values will be *in* bounds.

It turns out that this type of problem is kind of hard to solve in the world of sound. Consider a microphone in the real world. A microphone 
receives an analog signal of actual sound waves and converts them to an electrical signal. This signal is then fed to other systems, like 
amplifiers. The mic cannot send an *unbounded* electrical signal to an amplifier, but you cannot know ahead of time how loud the sound will be that
it receives. Therefore, it has to "clip" the electric signal to a range. The amplifier has to do the same thing when sending the signal to a
speaker; a speaker can only go *so loud*. This is why we have distortion so frequently in sound systems, like muffled output, or feedback.

Another unusual phenomenon is *cancellation*. We know that sound waves add together, but what do you get when you add `-1` and `1` together? You get
`0` of course. Sound waves have the ability to cancel each other out. Have you ever used noise-canceling headphones? This is essentially how they
work.

For now, let's take the easy way out of this problem: let's just lower the overall volume of our combined waves to a level that we know won't
exceed our assumed boundaries. Let's put this in `Volume.hs`:

```haskell
module WaveMachine.Audio.Volume where

import WaveMachine.Audio.Waves

applyVolume :: Double -> WaveFunction -> WaveFunction
applyVolume volume orig = \t -> (orig t) * volume
```

This will just multiply the result of a wave function.

OK, now we have a way to address our volume issue, so let's start to combine some waves. Earlier, I used `middleC` as just a constant that I looked
up on the Internet. My ultimate goal is to create *music*, so I want to work with the 12-tone scale that I'm used to. While looking into this, I
learned some interesting facts.

The standard "scale" that we're used to in Western music is arbitrary in some ways, but each tone is mathematically related to the next, at least
using the most common *twelve-tone even temperament* method of tuning. We start by assigning a set frequency to one tone. The system that was
standardized by the American music industry was for `A4`, the `A` above "middle C" to have a frequency of 440 Hz. Now, to derive the frequency of
the next tone, a "half-step" in music theory, we multiply (or divide to go down a half-step) by *the 12th root of 2*, which is the same as saying
2<sup>1/12</sup>. I'm not a math wizard, but I figured out how this works. Let's say we're going up a *whole* step. This would mean we multiply
twice: `n * 2^(1/12) * 2^(1/12)`. In algebra, we learn that `a^n * a^m = a^(n+m)`. Thus, going up a whole step is `n * 2^(2/12)`. An octave, being
12 half-steps, means `n * 2^(12/12)`, which simplifies to `n * 2^1`, which is the same as `n * 2`. So the notes in an octave are *twice* the
frequency of the notes in the octave below them. This might explain why two notes in the same octave sound so similar.

So basically, in a nut shell, the frequency for tones is a *geometric* series, with an even geometric increase between tones. What are these *tones*
I refer to? If you're already acquainted with basic music theory, you can skip this part. In Western music, each "octave" is made up of 12 tones,
which have these names:

> C C♯ D D♯ E F F♯ G G♯ A A♯ B

The tones wrap around; after `B`, the next highest tone is `C`. Each range of these tones is called an "octave." 
This "♯" character, which sort of resembles a pound/hash/octothorpe symbol, is called a "sharp." It means that the tone is a half-step above what
it follows. Why do some of these tones have a ♯ and others don't? It's because in a major scale in the key of C, which is all the white keys on a
keyboard, there are no sharps:

> C D E F G A B C

A scale generally means 8 tones, which is where "octave" comes from.

There is an alternate way to reference the "black keys" of a keyboard, with flat symbols (♭):

> C D♭ D E♭ E F G♭ G A♭ A B♭ B C

Flats are the opposite of sharps; they mean "a half-tone lower." A♭ and G♯ are equivalent; they are just different ways to express the same thing.

Sharps and flats are not easy to type on standard computer keyboards. Plus, they are not valid in Haskell symbols, so I'm going to define them in
a slightly different manner (`Pitch.hs`):

```haskell
data Tone = A | As | Bf | B | C | Cs | Df | D | Ds | Ef | E | F | Fs | Gf | G | Gs | Af

octaveIndex :: Tone -> Int
octaveIndex C  =  0
octaveIndex Cs =  1
octaveIndex Df =  1
octaveIndex D  =  2
octaveIndex Ds =  3
octaveIndex Ef =  3
octaveIndex E  =  4
octaveIndex F  =  5
octaveIndex Fs =  6
octaveIndex Gf =  6
octaveIndex G  =  7
octaveIndex Gs =  8
octaveIndex Af =  8
octaveIndex A  =  9
octaveIndex As = 10
octaveIndex Bf = 10
octaveIndex B  = 11
```

As you can see, I'm using an `s` and `f` as substitute suffixes for sharps and flats. I've also defined an `octaveIndex` to assign numeric values
for each tone; identical tones have the same index value.

We also need to know how far away from a *reference tone* a given tone + octave is. Since we're using the tuning method where `A4` is the reference
point, here's our index code:

```haskell
toneIndex :: Tone -> Int -> Int
toneIndex tone octave = 12 * (octave - 4) + ((octaveIndex tone) - 9) 
```

We multiply the octave by 12 because of the 12 tones, but offset from the reference octave of 4. We then add the octave index of the tone,
minus 9 because 9 is the index of `A`.

Now we're ready to calculate the *frequency* of a given tone:

```haskell
a4 = 440

toneFrequency :: Tone -> Int -> Double
toneFrequency tone octave = a4 * (2.0 ** ((fromIntegral $ toneIndex tone octave) / 12.0))

middleC = toneFrequency C 4

applyPitch :: Double -> WaveFunction -> WaveFunction
applyPitch pitch orig = \t -> orig (t * pitch)
```

The `**` is the exponent operator. All we have to do is multiply the reference tone by `2 ^ (n/12)`, where `n` is the offset from the reference.

Now we've got everything we need. Let's create a nice C major tonic chord (C + E + G) in `Main.hs`:

```haskell
import Data.ByteString.Lazy.Builder
import Data.Monoid
import System.IO
import WaveMachine.Audio.Pitch
import WaveMachine.Audio.Volume
import WaveMachine.Audio.Waves
import WaveMachine.Builders
import WaveMachine.Sampling

audioFn :: Double -> Double
audioFn = applyVolume 0.3 $ addWaves [
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
```

For our wave function, we apply our frequencies to three sine waves, add them together, then reduce the volume to 30%. When we try this out:

```bash
./build.sh && ./wave-machine | mplayer -
```

We now hear a chord!

I'm not really happy with this 30% volume business. I want to combine waves without worrying about the overall effect. In the next article, I'm
going to add code to automatically adjust the volume of the overall wave.
