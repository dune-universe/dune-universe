open Client

let test ~log (t : F.nominal_term) =
  Log.log_action log (fun () ->
    Printf.printf "Formatting the System F term...\n%!";
    let doc = PPrint.(FPrinter.print_term t ^^ hardline) in
    Printf.printf "Pretty-printing the System F term...\n%!";
    PPrint.ToChannel.pretty 0.9 80 stdout doc
  );
  let t : F.debruijn_term =
    Log.attempt log
      "Converting the System F term to de Bruijn style...\n"
      F.translate t
  in
  let _ty : F.debruijn_type =
    Log.attempt log
      "Type-checking the System F term...\n"
      FTypeChecker.typeof t
  in
  ()

