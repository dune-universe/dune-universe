# ORandForest

A pure OCaml implementation of a random forest classifier based on OC4.5. See
[Wikipedia](https://en.wikipedia.org/wiki/Random_forest#Algorithm) for more
information.

OC4.5 is an implementation of C4.5 that can be found
[here](https://github.com/tobast/OC4.5).

## Compiling

This project uses [OBuild](https://github.com/ocaml-obuild/obuild) as a
compilation manager.

To sum up, in order to get the project running,

```bash
    opam install obuild  # If you don't have it yet
    obuild configure
    obuild build
    obuild install
```

## Documentation

You can generate the documentation by running `ocamldoc`. However, you can have
a quick glance at the **not necessarily up-to-date** precompiled documentation
[here](https://tobast.fr/doc/ORandForest/). Same goes for OC4.5
[here](https://tobast.fr/doc/OC4.5/).

## Basic usage example

Assuming you're using integers (if not, use `Oc45.FloatOc45` and
`ORandForest.FloatRandForest` or reimplement the needed functions to functorize
`ORandForest.ORandForest` with your datatype, a basic session would look like
the following

* First generate a dataset;
```OCaml
    let trainSet = Oc45.IntOc45.emptyTrainSet
        nbFeatures nbCategories featuresContinuity in
    Oc45.IntOc45.addDataList trainDataPoints trainSet
```

* then tweak the dataset to your needs;

* then turn it into a random forest;

```OCaml
    let forest = ORandForest.IntRandForest.genRandomForest nbTrees trainSet in
```

* then classify

```OCaml
    let categories = List.map
        (ORandForest.IntRandForest.classify forest) testDataPoints in
```
