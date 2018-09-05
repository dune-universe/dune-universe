mkaudio
=======

[![Build status](https://travis-ci.org/johnelse/mkaudio.png?branch=master)](https://travis-ci.org/johnelse/mkaudio)

CLI program for generating audio files

Supported commands:

* `saw`, `sine`, `square`, `triangle`

Generate a waveform with a specified duration and frequency. Duration can be
specified in hours, minutes or seconds, or as a number of beats and a tempo in
BPM. For example:

Generate 5 seconds of saw wave:

```
mkaudio saw --frequency 440 --duration 5s sound.wav
```

Generate a sine wave lasting 4 beats at 120bpm (i.e. 2 seconds):

```
mkaudio sine --frequency 440 --beats 4 --tempo 120 sound.wav
```

Generate a square wave lasting half a minute:

```
mkaudio square --frequency 440 --duration 0.5m sound.wav
```

* `white-noise`

Duration is specified in the same way as for waveforms, e.g. to generate 10
seconds of white noise:

```
mkaudio white-noise --duration 10s sound.wav
```

* `beat`

Generate a simple beat from kick, snare and hi-hat patterns. Patterns are
specified as strings; each character which is `1` or `x` corresponds to a drum
hit; any other character corresponds to a lack of drum hit. Each character
represents one sixteenth note at the specified tempo. The `--repeats` parameter
allows generation of longer beats without typing in long repetitive patterns.

8 beats of eighth note hihats:

```
mkaudio beat --hihat 10 --repeats 16 --tempo 120 sound.wav
```

A simple syncopated beat lasting 4 beats:

```
mkaudio beat \
    --kick 1001000100100000 \
    --snare 0000100000001000 \
    --hihat 1111111111111111 \
    --tempo 120 sound.wav
```

For more information, and all optional parameters, see `mkaudio --help` and
`mkaudio <command> --help`.
