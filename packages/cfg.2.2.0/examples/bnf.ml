open Format
open Cfg
open Bnf_spec.Bnf
open Bnf_pp

let pp_level ppf (ts, grammar) =
  fprintf ppf "@[<2>Terminals:\n@\n%a@]\n@\n" pp_ts ts;
  let nts = grammar_contents grammar in
  fprintf ppf "@[<2>Partial grammar:\n@\n%a@]" pp_nt_map nts

let pp_bounded_level ppf n level =
  fprintf ppf "@\n@[<2>Level: %d\n@\n%a@]@\n" n pp_level level

let rec pp_bounded_levels_aux ppf n = function
  | [] -> ()
  | [level] -> pp_bounded_level ppf n level
  | level::levels ->
      pp_bounded_level ppf n level;
      pp_bounded_levels_aux ppf (n+1) levels

let pp_bounded_levels ppf = pp_bounded_levels_aux ppf 1

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let gr = Bnf_parser.start Bnf_lexer.start lexbuf in

  let ts_in_gr = ts_in_grammar gr
  and nts_in_gr = nts_in_grammar gr
  and prods_in_gr = prods_in_grammar gr
  and live_gr =
    try make_sane_live gr "S"
    with Not_found ->
      eprintf "No start symbol <S> specified! Aborting...@\n"; exit 1 in

  let gr_live_gr = grammar_of_live live_gr in

  printf "@[<2>Original grammar:\n@\n%a@]@\n@\n" pp_nt_map
    (grammar_contents gr);

  printf "@[<2>Terminals removed:\n@\n%a@]@\n@\n" pp_ts
    (TSet.diff ts_in_gr (ts_in_grammar gr_live_gr));

  printf "@[<2>Nonterminals removed:\n@\n%a@]@\n@\n" pp_nts
    (NTSet.diff nts_in_gr (nts_in_grammar gr_live_gr));

  printf "@[<2>Productions removed:\n@\n%a@]@\n@\n" pp_prods
    (ProdSet.diff prods_in_gr (prods_in_grammar gr_live_gr));

  printf "@[<2>Transformed grammar:\n@\n%a@]\n@\n" pp_nt_map
    (grammar_contents gr_live_gr);

  printf "@[<2>Derivation depth info:\n@\n%a@]@\n@\n@\n" pp_live_nts
    (deriv_depth_info live_gr);

  printf "@[<2>Depth-bounded derivation:\n%a@]@." pp_bounded_levels
    (bounded_grammar gr_live_gr "S" 4)
