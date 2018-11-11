The tests in this directory are organized as follows:
 - Folder `calc_prio` contains several implementations of a minimal calculator
   that can be used for performance tests.
 - Folder `calc_prio_no_pp` contains roughly the same tests as `calc_prio` but
   they are compiled witout relying on the syntax extension (the tests sources
   are already preprocessed).
 - Folder `calc_yacc` contains an `ocamlyacc` implementation (not compiled and
   tested) that can be used for comparing performances.
 - Folder `regexp` contains tests related to regular expressions together with
   some performance comparison with native `Str` (10 times faster).
