SameBits
========

This private library is only to be used for testing purposes. It provides
facilities to check that two modules implementing a typical Random interface
behave in fact in the same way. Using it is simply a matter of depending on it
and creating a test executable of the form:

```ocaml
SameBits.run
  first_name  (module FirstModule)
  second_name (module SecondModule)
```

This runs tests on all the various functions in a Random interface, checking
systematically that the output is the same. The tests might be degenerate if the
modules themselves are inconsistent (eg. if their state-manipulation functions
are broken).
