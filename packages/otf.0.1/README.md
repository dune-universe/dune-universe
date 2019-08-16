otf is a simple Output Test Framework.

The goal is to test command lines and check that it produces the
correct output (exit code, standard output, standard error output).


Usage examples
--------------

The tests and example directories contain several tests.

Once otf is compiled, you can for example run one simple test by using
the following command:

    % ./otf examples/false
    ------ examples/false -----------------------------
    
    ===================================================
    Summary: 1 passed, 0 failed
    ===================================================

To check the configuration of a particular test, you can use the
-print-config command:

    % ./otf -print-config examples/grep
    ARGS: string list = ["foo"] [from "examples/grep.test"]
    DIFFOPTIONS: string list = [] [from default configuration]
    EXIT_CODE: int = 0 [from default configuration]
    PROG: string = "grep" [from "examples/grep.test"]
    Config OK

otf supports simple collections, with -run-collection on a directory
containing tests:

    % ./otf -run-collection examples
    ===================================================
    Running collection "examples"
    6 tests collected
    
    failing F
    false .
    grep .
    grep2 .
    grep3 .
    sort .
    
    ===================================================
    ------ examples/failing ---------------------------
    Invalid exit code (got 0 while expecting 1)
    
    ===================================================
    Summary: 5 passed, 1 failed
    ===================================================

Finally, it is possible to factor config elements for multiple tests
by placing them in the same directory, and by writing the common
config elements in config.otfrc in the directory. There is an example
in tests/common-config.