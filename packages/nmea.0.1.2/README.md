# NMEA sentence parser
An NMEA sentence parser for OCaml.


## Install

```opam install nmea```

## Usage

```ocaml
let rec loop_coord ch = match Nmea.Parse.next_coord with
| None -> ()
| Some(c) -> Printf.printf "Position: %s\n" c; loop_coord ch
in loop_coord (open_in "/dev/ttyACM0");;
```

## Supported sentences

- GPGGA: Essential fix data which provide 3D location and accuracy data
- GPRMC: Recommended Minimum GPS PVT
- GPGLL: Geographic Latitude and Longitude
- GPGSV: Satellites in View
- GPGSA: GPS DOP and active satellites

## Roadmap

1. ~~GPS related sentences~~
2. Wind / Heading related sentences
3. WP related sentences
4. AIS related sentences


## License

```
Copyright (c) 2020 Davide Gessa

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
```