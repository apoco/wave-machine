---
title: Amplitude Auto-Adjustment
layout: post
---

In the previous article, we learned that because of the ways that sound waves combine, the resulting wave's amplitude can increase. This is
inconvenient for us because our sampling code assumes a range of -1 to 1. I worked around this by multiplying the total wave by 0.3, knowing that
should cover things, but I want to combine sounds without worrying about it going forward.

Thankfully, this is easy to remedy; all we have to do is take the highest amplitude of the wave and divide the whole thing by that amount. We'll
put that in our sampling function:

```haskell
sampleInt16 :: WaveFunction -> Int -> Double -> [Int16]
sampleInt16 audioFn frequency duration = 
    [floor (v * 32767.5 / maxMagnitude) | v <- samples]
    where 
        samples = sample audioFn frequency duration
        maxMagnitude = maximum (map abs samples)
```

This places our samples back in bounds. Now we can simplify our `Main.hs` audio function:

```haskell
audioFn :: WaveFunction
audioFn = addWaves [
    applyPitch (toneFrequency C 4) sineWave,
    applyPitch (toneFrequency E 4) sineWave,
    applyPitch (toneFrequency G 4) sineWave ]
```

To see the code for this article, check out the [article-4](https://github.com/apoco/wave-machine/tree/article-4) branch.

Next, I'm going to going to add *sequencing* to my experiment code.
