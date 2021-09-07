

![Build](https://github.com/lindig/fit/workflows/CI/badge.svg)

# FIT

This is a minimal [OCaml] project to parse FIT files as they are
produced by personal fitness devices. FIT is a binary format that groups
basic values in records, which typically include a timestamp.

    {
      "msg": "20",
      "13": 11,
      "2": 1900,
      "5": 1732414,
      "6": 0,
      "1": 1669620,
      "0": 622905943,
      "253": 971857351
    }

Each record has a global message number (20) which defines the purpose
of the record and a number of values in position slots. The meaning of
these is defined in the FIT Protocol but this library (so far) only
implements the parsing. For example, 20 message is called _record_ in
the FIT protocol and slots have these meanings:

* 0 position\_lat
* 1 position\_long
* 2 altitude
* 5 distance
* 6 speed
* 13 temperature
* 253 timestamp

Values are further scaled and shifted, which is also defined in the
protocol, and this transformation is only implemented for a few fields
of the "record" message:

    {
      "msg": "record",
      "timestamp": "2020-10-17T06:25:19",
      "0": 622927716,
      "1": 1703145,
      "speed": 2.745,
      "distance": 277.23,
      "altitude": -118.8,
      "temperature": inf
    }

# CLI

The `fit` command emits the data to stdout in JSON format. I am using
this currently for inspecting FIT files. The FIT file in `data/` is from
a bike computer.

    $ fit data/xpress-4x-2020-10-17.fit | head -25
    [
      {
        "msg": "0",
        "3": 5122,
        "4": 971850094,
        "1": 267,
        "2": 1803,
        "5": 0,
        "0": 4
      },
      {
        "msg": "68",
        "0": 17,
        "1": 1,
        "2": 0,
        "3": 10,
        "4": 255,
        "5": 0,
        "6": 0,
        "7": 0,
        "8": 232,
        "9": 0,
        "10": 60,
        "11": 19693
      },
      ...

# Using FIT

This is work in progress and has not been published as an official
[Opam] package. Consider pinning it to make it available in your local
Opam installation.

    opam pin add -y git+https://github.com/lindig/fit

Now it is available:

    $ utop -require fit -require rresult
    utop # open Rresult;;
    utop # Fit.read "data/xpress-4x-2020-10-17.fit" >>= fun fit ->
           Fit.to_json |> R.return;;


# Resources

* https://developer.garmin.com/fit/protocol/
* https://www.pinns.co.uk/osm/fit-for-dummies.html
# Contribute

If you find this useful, please contribute back by raising pull
requests for improvements you made.

[OCaml]:  https://www.ocaml.org/
[Opam]:   https://opam.ocaml.org/
