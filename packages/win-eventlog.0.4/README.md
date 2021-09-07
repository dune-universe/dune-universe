Bindings to the Windows event log
=================================

[![Build status](https://github.com/mirage/ocaml-win-eventlog/actions/workflows/main.yml/badge.svg)](https://github.com/mirage/ocaml-win-eventlog/actions)

This library allows you to log via the Windows event log from OCaml programs.

A low-level example:

```ocaml
let log = Eventlog.register "Mirage.exe" in
let category = 0 and event = 1 in
Eventlog.report log `Success category event [|
  "insertion string 1";
  "insertion string 2";
|]
```

You may wish to use the Log reporter interface instead:

```ocaml
let log = Eventlog.register "Mirage.exe" in
Logs.set_reporter (Log_eventlog.reporter log ());

Log.err (fun f -> f "This is an error");
Log.info (fun f -> f "This is informational");
Log.debug (fun f -> f "This is lowly debugging data");
```

For more context, please read the [MSDN ReportError example](https://msdn.microsoft.com/en-us/library/aa363680(v=vs.85).aspx).

Please note that this code will compile on non-Windows platforms, but this
is for debugging only.
