Visual Metronome and Band Conductor
===================================

**Vimebac** is graphical metronome and instructions display that interfaces with
JACK-midi applications.  The display can be completely driven by MIDI events and
it can also send MIDI events.

It can also be self-driven and hence run without `jackd` although this is
somewhat less interesting since it becomes *just* a visual metronome.

The current features are:

- A visual metronome with arbitrary time-signatures.
- The display of arbitrary lines of text, e.g. to give instructions to the
  musician(s).
- Custom keyboard bindings to send arbitrary MIDI events.

Build
-----

Simply:

    jbuilder build vimebac.exe
    
Usage
-----

See:

    vimebac --help

You can always quit the application with `Ctrl-Q`.

### Self-Driven Mode

#### Examples

Start at 106 beats per minute, the default time-signature is 4/4 showing also
eighths:

    vimebac self --bpm 106

<div><a href="https://user-images.githubusercontent.com/617111/34076342-665809c4-e2b1-11e7-832c-fb829c31f78a.gif"><img
  src="https://user-images.githubusercontent.com/617111/34076342-665809c4-e2b1-11e7-832c-fb829c31f78a.gif"
></a></div>

Or with a slow, standard, 9/8 time-signature:

    vimebac self --bpm 70 --beats 3 --subdivision 3

<div><a href="https://user-images.githubusercontent.com/617111/34076346-b3c4eb32-e2b1-11e7-811d-b5c940b1dadc.gif"><img
 width="80%"
  src="https://user-images.githubusercontent.com/617111/34076346-b3c4eb32-e2b1-11e7-811d-b5c940b1dadc.gif"
></a></div>

#### Keyboard Bindings

Then there are a few bindings that allow to modify the tempo:

- `j` → remove 1.
- `k` → add 1.
- `h` → remove 10.
- `l` → add 10.

And also to modify the font size:

- `-` → multiply by 0.9
- `=` → multiply by 1.1


### Jack-Driven Mode

#### Example

Start and connect to Jack as `vimebac-00`:

    vimebac jack --jack-name vimebac-00
    
You need then to connect the output of a MIDI-generating application into the
`vimebac-00:in`.

<div><a href="https://user-images.githubusercontent.com/617111/34076364-4be3839c-e2b2-11e7-8221-e8f3b59d5879.png"><img
 width="60%"
  src="https://user-images.githubusercontent.com/617111/34076364-4be3839c-e2b2-11e7-8221-e8f3b59d5879.png"
></a></div>

#### MIDI Protocol

Vimebac operates the “channel” and the 2 “data-bytes” of any input MIDI event
(resp. `chan` `dat1`, and `dat2`).  For now it doesn't look at the “status.”

Here is how the values are interpreted (values not mentioned are ignored):

```
chan: 0            → beat_progress <- dat1 * 127 + dat2 / (127 ^ 2 + 127)
chan: 1            → bar_progress <- dat1 + (dat2 / 127)
chan: 2 && dat1: 0 → beat_on dat2
chan: 2 && dat1: 1 → beat_off
chan: 3            → bpm <- dat1 * 127 + dat2
chan: 4 && dat1: 0 → bar_structure.length <- dat2
chan: 4 && dat1: 1 → bar_structure.(dat2) <- Strong
chan: 4 && dat1: 2 → reset_bar_structure
chan: 5 && dat1: 0 → text_lines.(dat2) <- empty
chan: 6 or 7       → text_lines.(bits 6--3 of dat1)
                               .(bits 2--0 of dat1 ^ bits 6-4 of dat2)
                               .(if chan = 7 then (4 higher bits) else (4 lower bits)
                           <- bits 3--0 of dat2

```

where:

- `beat_progress` is the top progress bar (float in [0, 1]),
- `bar_progress` is the needle progress in beats (float in [0, 128]),
- `beat_on <n>` highlights the `n`-th bullet,
- `beat_off` turns off all bullet highlighting,
- `bpm` is the internal tempo (it is for display in jack-driven mode),
- `bar_structure.length` is the number of bullets, all “weak” by default,
- `bar_structure.(<n>)` is the `n`-th bullet (that can be set to “strong”),
- `reset_bar_structure` sets all the bullet to “weak.”
- `text_lines` is the array of lines to display.

### Display Control


One can set all the geometry of the window with CLI options, for instance:

```
vimebac self --bpm 80 \
    --width 800 \
    --height 500 \
    --x-coord 250 \
    --y-coord 42
```

The relative size of the metronome Vs the text-area is set
with `--x-percent-metronome` (percentage of the total width):

```
vimebac self --bpm 80 --x-perc 15
```

One can also set the initial size of the font (percentage of the total
height):

```
vimebac self --text-size 25
```

The tempo display can be disabled:

```
vimebac self --hide-bpm
```
