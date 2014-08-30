---
title: My First Sine Wave
layout: post
---

In the previous article, I went over the basics of how sound works and the anatomy of digital sound. Now it's time to get down with some code and generate a sound wave.

I am using the Haskell programming language. I know that Haskell is a bit of an esoteric language, and I'm not very experienced with it. Because of this, I won't go into a lot of detail explaining the language, but I'll try to clarify some of the more confusing aspects. I chose Haskell because of its elegance, amazing type system, and mathematical style. Sound generation libraries already exist for this language, but I'm going to do as much as I can from scratch for the sake of learning.

A sound wave can be thought of as a mathematical function that maps a given time to a value representing air pressure. The function should be periodic, meaning it repeats; if it didn't repeat, then it would have no frequency, and thus there would be no tone. The function should also produce values that fall within a limited range; otherwise, the mapping of the sampled values to a discrete number of bits would be impossible.

A perfect pre-made function from the world of trigonometry that fits these parameters is *sine*. Sine produces a range of -1 to 1. It usually operates on an angle parameter; since angles "wrap around" (361 degrees is the same as 1 degree), sine repeats for every 360 degrees; more typically, radians are used instead of degrees, so using radians, the function repeats every 2ᴨ. If you look at a graph of sine in the previous article, you'll see it has a nice curving pattern; this produces a smooth, simple sound.

Let's use sine to produce our first sound clip. One modification I want to make to the function is to make it more convenient for working with time. Because sine operates on angles and repeats every 2π units, time is expressed in seconds, having the wave repeat for every second makes it much easier to deal with dealing with Hz.

Doing this is easy:

```haskell
sineWave :: Double -> Double
sineWave t = sin (t * 2 * pi)
```

The first line defines the type of the function, `Double -> Double`. What this means is that the function takes a `Double` (double-precision floating point number) and returns another `Double`. `Doubles` are high-precision 64-bit numbers that most computers are efficient at working with. We'll use this type of number to represent both time and pressure values. The second line of the function gives the actual calculation.

Because sine has a range of -1 to 1, and because of its mathematical convenience, we'll have 1 represent the maximum and -1 represent the minimum of any sound function going forward. For the time component, each unit will represent a second.

Our function above repeats every second. In other words, it has a frequency of 1 Hz, or cycle per second. 1 Hz is very slow, so this by itself would be a very low-pitch sound. According to Wikipedia, humans normally can't hear anything lower than 20 Hz, so this wouldn't be audible.

We have the waveform in our `sineWave` function, but we also need a way to control the frequency. Just as we increased the frequency of the sine function by multiplying the time parameter, we can multiply it more to make a higher pitch. This helper function will do:

```haskell
applyPitch :: Double -> (Double -> Double) -> (Double -> Double)
applyPitch pitch orig = \t -> orig (t * pitch)
```

This function takes a frequency (in Hz) and an audio function and returns a new audio function that outputs at a higher frequency. So what do we use for the pitch parameter? Since we're going to eventually be creating music, let's use a concert pitch "middle C" as on a piano. Here is the frequency of middle C:

```haskell
middleC = 261.63
```

Now, if we apply this to our wave function, we should get a sine wave tone at middle C:

```haskell
audioFn = applyPitch middleC sineWave
```

Before we can output this as digital sound, we'll need to now *sample* the audio function. As explained in the previous article, this means we need to produce a discrete number of values over time. Let's start with producing the time values for our samples: 

```haskell
sampleTimes :: Double -> Double -> Double -> [Double]
sampleTimes frequency start end
    | start > end = []
    | otherwise = start:(sampleTimes frequency (start + (1.0 / frequency)) end)
```

This function takes a given frequency in Hz, start time, and end time, and produces a list of Doubles. If the start time is past the end time, there are no sample times to be produced, so we return an empty list. Otherwise, we return the starting time followed by the remaining times, which we get by running the same function, only incrementing the start by one time increment. Eventually, as we keep repeating the function, we'll increment the start time past the end time and will end with the empty list.

If you've only done imperative-style programming, this function may seem odd to you; why would you produce a list of time values instead of just looping? In Haskell, a list is the equivalent of a loop because the values in the list are produced lazily; we aren't filling an array of time values, but rather are setting up a sequence that will be created as we consume it. This is also why we do not have to worry about having a stack overflow with a recursive function.


Now that we can produce the sequence of sampling times, we can produce the samples themselves:

```haskell
sample :: (Double -> Double) -> Double -> Double -> [Double]
sample audioFn frequency duration = [audioFn t | t <- sampleTimes frequency 0 duration]
```

Our sample function takes an audio function, frequency, and duration, and returns a list of `Double` sample values. To do that, we apply `audioFn` to each time that we get from `sampleTimes`, using 0 as the starting time and the duration as the ending time.

You'll recall from the last article that when creating digital samples, we need to select a sampling rate and bit depth. Let's go for CD quality. Compact discs have a sample rate of  44,100 Hz and bit depth of 16. Our sample function above produces Double values for samples, which are 64-bit. We're going to need to shed some precision.

We're starting with decimal values ranging from -1 to 1, and we need to convert them to 16-bit integer values ranging from -32768 to 32767. Haskell has a data type called `Int16` that fits our needs, but we'll need to import the module that contains that type first:

```haskell
import Data.Int
```

Now that we have that, here's a version of our sample function that produces `Int16`s instead of `Double`s:

```haskell
sampleInt16 :: (Double -> Double) -> Double -> Double -> [Int16]
sampleInt16 audioFn frequency duration = 
    [floor (v * 32767.5) | v <- samples]
    where samples = sample audioFn frequency duration
```

For each sample value, we multiply it by 32767.5, which gives us a range of -32767.5 to 32767.5. Finally, we apply `floor`, which rounds fractional values down to the next lowest integer, giving us the -32768 to 32767 we are after.

Now we can create the samples for our audio function:

```haskell
samples = sampleInt16 audioFn 44100.0 5 
```

With our `audioFn` sine wave, sampling rate of 44,100, and duration of 5 seconds, we'll have a 5 second long tone at middle C, sampled at CD quality.

Now how are we going to listen to this? Ideally, we would output this to an audio file that we could open with our favorite music player, but this post is already really long, and audio file formats deserve their own articles. So for now, let's produce a `raw` file containing just our samples.

Before we proceed, I'm going to have to explain a really abstract concept used in algebra and Haskell that could seem a little intimidating. A `Monoid` type is a data type that has a function for combining two values of its type to produce a new value of the same type. Monoid types also have a defined `identity` value which, when combined with another value of the type, does not changes its value.

Here's one example: we could have an `Addable` type that is a `Monoid`; the function for combining two values is the `+` operator, and the identity value is 0, since adding 0 to a number doesn't change the number. `Multipliable` could be another example; The `*` operator combines the values, and 1 is the identity, since it doesn't modify a value. An actual Haskell `Monoid` that we've already used is `List`; it has a `++` operator that joins two lists, and the value `[]` is the identity.

I bring up Monoids because we're going to use one called a `Builder`. A `Builder` simply describes an operation for producing a sequence of bytes. Two `Builder`s can be combined to make a new `Builder` that joins these sequences of bytes. The identity for `Builder` is a `Builder` that produces no bytes.

Before we can start using `Builder` and `Monoid` functions, we'll have to import them from these modules:

```haskell
import Data.ByteString.Lazy.Builder
import Data.Monoid
```

Now let's create our builder:

```haskell
audioBuilder :: Builder
audioBuilder = mconcat sampleBuilders
    where sampleBuilders = map int16LE samples
```

The `mconcat` function used here is a function that takes a list of `Monoid`s and combines them all into a new `Monoid`. In this case, we're combining a list of `Builder`s called `sampleBuilders`. The `map` function simply applies a function to each element in a list, producing a list of the results; in this case, the list is samples, which are our samples that we produced earlier. So what is `int16LE`? This is a function that creates a `Builder` which outputs the bytes for an `Int16`, like our samples, in little-endian order. Little-endian means that the least-significant bytes come before the most-significant.

That's a lot to absorb for one function. To state it simply, this creates a builder that outputs the bytes for each of our samples.

We're about ready to write our last Haskell function for this article. Our program needs its `main` function, the function that is invoked when you run it. But before I can do that, I need to discuss I/O in Haskell programming.

Haskell is known as a *purely functional programming language*. What does this mean? Most programming languages have "functions," but they don't typically mean the same thing as *function* in mathematics. In the world of math, a function is a mapping of an input value to an output value. For a given input, the output will always be the same, which is highly advantageous in computing. Haskell functions are like mathematical functions.

In contrast, non-purely-functional programming languages use a different sense of the term function, meaning a *subroutine*, or series of computing instructions. This is not the same thing. Like a function, a subroutine has input, computation, and output, but it can also read from or modify external state.

The tricky thing about purely functional languages is that any useful program will need to modify external state in order for you to receive its output, whether it's a screen, file system, or audio device. And most programs (not ours) responds to external input from some source, whether it's a keyboard or an Internet website. These are collectively known as I/O operations (input/output).

So how do you write a program that needs to perform I/O in a pure programming language? The approach in Haskell is to separate the mathematical *definition* of a computation from its *execution*. Thus, the language remains pure, but the runtime engine is not. A computation that performs I/O can be represented with an `IO` `Monad`. Monads are a challenging topic which I thankfully don't need to elaborate on for this simple program.

To start, let's import a module to support the I/O we need to perform:

```haskell
import System.IO
```

And now, let's write our main function:

```haskell
main :: IO ()
main = hPutBuilder stdout audioBuilder 
```

The `IO ()` type of our function simply means that it's an `IO` Monad that returns no value. The `hPutBuilder` function takes a `Handle` and a `Builder` and has the builder write its output to the handle. This `stdout` value (pronounced "standard out"), if you're not versed in Unix-like systems, is the handle for the main output stream of a program. If we ran our program in a console window, the output would be displayed on the screen.

That's it for our code. To see it all put together and organized, you can [browse through the code](https://github.com/apoco/wave-machine/tree/article-1). If you want to get the code on your local machine and build the code for this article, you must have `git` and `ghc` installed, then run the following in a `sh`-compatible shell:

```bash
git clone https://github.com/apoco/wave-machine
cd wave-machine
git checkout article-1
sh build.sh
```

Now you can run the generated `wave-machine` executable. But wait! Don't just run it! It will spew garbage all over your terminal. Pipe the content into a file, like this:

```bash
./wave-machine > sine.raw
```

This will give us a binary file containing nothing but our raw samples. Not very exciting. I don't know of any audio players that can play a raw file.
At a later time, we can update the code to write to a real audio file, but for now, let's use a program called sox to convert this raw file to a WAV.

```bash
sox -b 16 -e signed-integer --endian little -r 44100 sine.raw sine.wav
```

You can play the file here:

<audio src="{{site.url}}/audio/first-sine-wave.wav" controls/>

In the next article, I'll help us avoid this inconvenient step and write out a wave file directly.
