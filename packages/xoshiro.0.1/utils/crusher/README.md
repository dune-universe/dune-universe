Crusher
=======

This private library is only to be used for testing purposes. It provides
facilities to run [*Crush](http://simul.iro.umontreal.ca/testu01/tu01.html) test
batteries on any [bits] function. Using it is simply a matter of depending on it
and creating a test executable of the form:

```ocaml
Crusher.run ~name bits
```

The obtained executable recognises a few command line arguments. By default
(used in continuous integration, for instance), it only runs SmallCrush.
