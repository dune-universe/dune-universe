open Pa_ocaml_prelude

#define LOCATE locate

module Ext = functor(In:Extension) ->
struct
  include In

  let extension (alm,lvl) = parser
  | STR("do") e:(expression) STR("where") r:STR("rec")? b:let_binding ->
      if r<>None then
              <:expr<let rec $bindings:b$ in $e$>>
            else
              <:expr<let $bindings:b$ in $e$>>

  | STR("let") STR("try") b:let_binding
    STR("in") e:(expression) STR("with") c:(match_cases) ->

    (* missing quotation in pattern yet *)
       let c = List.map
           Parsetree.(fun ({ pc_rhs = e; _ } as b) ->
              { b with pc_rhs = <:expr< fun () -> $e$ >> }) c
       in
        <:expr<(try let $bindings:b$ in fun () -> $e$ with $cases:c$) ()>>

  let extra_expressions = extension::extra_expressions
  let _ = Pa_lexing.add_reserved_id "where"

end

module M = Pa_main.Start(Pa_ocaml.Make(Ext(Initial)))
