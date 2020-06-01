
 
  $ "$TEST_DIR"/command_rpc_example.exe caller -version v1 10 11
  result: 21

  $ "$TEST_DIR"/command_rpc_example.exe caller -version v2 10 11
  result: 21

Using a program name that is not absolute but instead should be resolved
relative to PATH.

  $ "$TEST_DIR"/command_rpc_example.exe caller -version v2 10 11 -method via_bash
  result: 21

Check that we get good error messages when we
try to execute a non-existent or non-executable file.

  $ "$TEST_DIR"/command_rpc_example.exe caller -version v2 10 11 -method '(binary ./does-not-exist)'
  (Unix.Unix_error "No such file or directory" Core.Unix.create_process
   "((prog ./does-not-exist) (args (v2-implementation)) (env (Extend ())))")
  [1]

  $ touch not-executable

  $ "$TEST_DIR"/command_rpc_example.exe caller -version v2 10 11 -method '(binary ./not-executable)'
  (Unix.Unix_error "Permission denied" Core.Unix.create_process
   "((prog ./not-executable) (args (v2-implementation)) (env (Extend ())))")
  [1]



We have to print stdout and stderr of this command separately because
it can't give any ordering guarantee
  $ STDOUT=$("$TEST_DIR"/command_rpc_example.exe caller -version v3 10 11 2>/dev/null)

  $ echo "$STDOUT"
  result: 21

When run in [-sexp] mode, the implementation command is usable from terminal
  $ echo '((rpc_name command-rpc-demo) (version 3) (query (3 6)))' | "$TEST_DIR"/command_rpc_example.exe v3-implementation -sexp 2>&1
  hello world via Core
  hello world via Async
  hello world via Async stderr
  hello world via fork&exec
  9 

And it routes stderr and stdout properly
  $ echo '((rpc_name command-rpc-demo) (version 3) (query (3 6)))' | "$TEST_DIR"/command_rpc_example.exe v3-implementation -sexp 2>/dev/null
  9 

  $ echo '((rpc_name spawn-sleep-1000-and-print-its-pid-to-fd-7) (version 1) (query ()))' | $TEST_DIR/command_rpc_example.exe spawn-sleep-1000-and-print-its-pid-to-fd-7 7>seven.tmp -sexp 2>/dev/null | { cat; kill "$(cat seven.tmp)"; }
  ()

Expert interface works as intended
  $ "$TEST_DIR"/command_rpc_expert_example.exe caller 19 23
  42
