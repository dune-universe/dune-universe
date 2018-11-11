Rosetta - universal decoder of an encoded flow to Unicode
-----------------------------------------------------

Rosetta is a merge-point between [uuuu](https://github.com/dinosaure/uuuu),
[coin](https://github.com/dinosaure/coin) and
[yuscii](https://github.com/dinosaure/yuscii). It able to decode UTF-7, ISO-8859
and KOI8 and return Unicode code-point - then, end-user can normalize it to
UTF-8 with [uutf](https://github.com/dbuenzli/uutf) for example.

The final goal is to provide an universal decoder of any encoding. This project
is a part of Mr. MIME, a parser of emails.

If you want to handle a new encoding (like, hmmhmm, APL-ISO-IR-68...), you can
make a new issue - then, the process will be to make a new little library and
integrate it to `rosetta`.
