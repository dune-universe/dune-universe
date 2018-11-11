open Pa_ocaml_prelude

module Ext = functor(In:Extension) -> 
struct
  include In
  (* can not use both pa_glr and pa_ocaml extension yet! Waiting for bootstrap ! *)
  (* need Decap.apply to avoid to parse the quotation when the grammar is not ready.
     will be automatic when using a syntax for parser *)

  let test = parser
  | STR("do") e:(expr) STR("where") r:STR("rec")? b:let_binding ->
      (Let, if r<>None then <:expr<let rec $b$ in $e$>> else <:expr<let $b$ in $e$>>)
  | STR("try") e:(expr) STR("with") c:math_cases STR("finally") e':(expression_lvl Let) ->
      let c = List.map (fun (pat, e) -> (pat, <:expr< fun () -> $e$ >>)) c in
      (Let, <:expr<(try $e$; fun () -> $e'$ with $c$) ()>>)
								     
  let extra_expressions = test::extra_expressions
  let reserved_ident := "finally"::"where"::!reserved_ident

end

(* Creating and running the extension *)
module PatolineDefault = Pa_ocaml.Make(Ext(Pa_default.ParserExt))
module M = Pa_main.Start(PatolineDefault)

