`hasse` - Random Generation of Hasse-Diagrams
=============================================

Usage
-----

To run this example you need to install the dot-utility first, which you can
find in the [Graphviz](http://www.graphviz.org) distribution.  This utility
can draw directed and undirected graphs given a human-readable specification.

After compilation of the `hasse.native`-binary (make), just run it, possibly setting
some options about which you can learn by entering:

```sh
hasse.native --help
```

Running `hasse.native` by itself will print a random graph-specification as
required by the `dot`-utility to standard output.  If you want to visualize
this graph, just redirect the specification to some file to be read by `dot`
or pipe it to this utility directly, e.g.:

```sh
hasse | dot -Tpdf -o foo.pdf
```

This will generate a nicely rendered PDF-file `foo.pdf`, which you can
visualize with your favorite viewer.

Play around with some command-line options to get a feeling for partially
ordered sets.  The implementation of the `hasse`-tool is very simple and can
be easily extended to render any kind of partially ordered structure with the
`dot`-utility.

---------------------------------------------------------------------------

Contact Information and Contributing
------------------------------------

In the case of bugs, feature requests, contributions and similar, you can
contact me here: <markus.mottl@gmail.com>

Up-to-date information should be available at: <http://mmottl.github.io/pomap>

Enjoy!

Markus Mottl on July 10, 2012
