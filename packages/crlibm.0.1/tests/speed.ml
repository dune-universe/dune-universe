open Benchmark

let () =
  let x = 10. in
  let fns = ["cos", cos, x;
             "exp", exp, x;
             "log", log, x;
             "cr-cos", Crlibm.cos, x;
             "cr-exp", Crlibm.exp, x;
             "cr-log", Crlibm.log, x ] in
  let samples = throughputN ~repeat:5 1 fns in
  tabulate samples
