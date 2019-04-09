# ocaml-scgi

[SCGI](http://www.python.ca/scgi/protocol.txt) implementation in OCaml

## Dependencies

* [LWT](http://ocsigen.org/lwt/install) -- initial version uses 2.3.1

You could just copy the source into your own project.  To build a library, you'll also need:

* [findlib](http://projects.camlcity.org/projects/findlib.html)

Note that LWT has dependencies too, including:

* [libev](http://dist.schmorp.de/libev/)
* [react](http://erratique.ch/repos/react.git)

## Building

    $ make
    $ make install
    $ make test
    $ make samples

(Alternatively, run ``make world`` to do all of above)
