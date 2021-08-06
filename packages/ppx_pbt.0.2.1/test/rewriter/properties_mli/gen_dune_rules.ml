let output_stanzas filename =
  let base = Filename.remove_extension filename in
  Printf.printf
    {|
(rule
 (targets %s.expected.format)
 (deps (:pp pp.exe) (:input %s.expected.ml))
 (action (bash "./%%{pp} --impl %%{input} -o %%{targets}")))

(rule
 (targets %s.actual)
 (deps (:pp pp.exe) (:input_ml %s.ml) (:input_mli %s.mli))
 (action (bash "
     ./%%{pp} --intf %%{input_mli} &&
     ./%%{pp} --impl %%{input_ml} -o %%{targets}")))

(rule
 (alias runtest)
 (deps (:actual %s.actual) (:expected %s.expected.format))
 (action (diff %%{expected} %%{actual})))

(test
 (name %s)
 (modules %s)
 (preprocess (pps ppx_pbt)))
|}
    base
    base
    base
    base
    base
    base
    base
    base
    base

let is_error_test = function
  | "pp.ml" -> false
  | "gen_dune_rules.ml" -> false
  | filename ->
      Filename.check_suffix filename ".ml"
      && (not (Filename.check_suffix filename ".expected.ml"))
      && not (Filename.check_suffix filename ".pp.ml")

let () =
  Sys.readdir "." |> Array.to_list |> List.sort String.compare
  |> List.filter is_error_test |> List.iter output_stanzas
