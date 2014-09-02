---
title: Homophony
layout: post
---

In the last article, I produced a melody. In this article, I wanted to add a simple base line. This ended up being pretty easy. I simply created
a second sequence of notes so that we now have two parallel sequences:

```haskell
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
```

Then, I simply sequence both, and add the two audio waves together:

```haskell
samples :: [Int16]
samples = sampleInt16 (addWaves rhAudio lhAudio) sampleRate dur
    where 
        (rhAudio,dur) = audioSeq rhNotes
        (lhAudio,  _) = audioSeq lhNotes!
```

Check out the [article-6](https://github.com/apoco/wave-machine/tree/article-6) branch to see the code in its entirety. When run, we get this audio
clip:

<audio src="{{site.url}}/audio/homophony.wav" controls/>

When I first listened to this, I thought I had screwed up; I didn't hear the base line at all. You also may not be hearing it. However, when I
listened more carefully, I noticed that it was indeed playing, but was very quiet. When I listen to the same clip with my headphones, I can hear it
just fine. What's going on? Are my laptop speakers just crappy?

The speakers are only part of the story. The ability of speakers to produce sound at different frequencies does greatly vary. My laptop speaker is
pretty bad at lower frequencies. A sub-woofer, on the other hand, is specifically tailored to those lower frequencies. Speakers can also have flaws
that will distort sound. The acoustics of the room you're in also affects the quality of the sound. With headphones, the shorter distance to the ear
canal means less distortion through room acoustics.

Another factor that makes the lower frequency quieter has to do with the 
[equal loudness contour](http://en.wikipedia.org/wiki/Equal-loudness_contour). Our ears are simply better at picking up some frequencies over others.
As I mentioned in an earlier article, some frequency ranges are completely inaudible to human ears. The "loudness" of the audible frequencies vary
widely from person to person, especially when hearing impairment is added to the mix. The Equal-loudness contour shows the average relation of
decibels and frequency to loudness (given in _phon_ units):

![Equal-loudness contour]({{ site.url }}/images/Lindos1.svg)

Not being an audiophile, I have always been annoyed that sound systems have treble and bass adjustment; can't they just preset it to the "right"
setting? Now I realize that "right" isn't so straightforward; you really do need to adjust these settings for different speaker and room setups.

The fact that we're using sine waves exacerbates the issue. Because of that simple wave form, you don't have and _harmonics_.
Harmonics, a.k.a. _harmonic partials_ are additional oscillations taking place at frequencies that are multiples of the _fundamental_ frequency.
The higher-frequency harmonics, which the ear is more sensitive toward, normally help you notice the sound more easily. In the next article, I'm
going to explore harmonics by stepping away from sine waves for a bit.
