open Pa_ocaml_prelude
  
#define LOCATE locate

(*
  ocamlopt -pp pa_ocaml -I .. -I +compiler-libs ocamlcommon.cmxa str.cmxa unix.cmxa decap.cmxa decap_ocaml.cmxa pa_mu4.ml -o pa_mu
*)
module Ext = functor(In:Extension) -> 
struct
  include In

  let cont = "kk"

  let gen_sym =
    let cont = ref 0 in
    (fun name ->
     let c = !cont in
     cont := c+1;
     name ^ string_of_int c)

  let extension = parser
    | (key_word "cfun") id:lowercase_ident STR("->") e:(expression_lvl Let) ->
      Let, <:expr< ($lid:cont$ (fun $lid:id$ $lid:cont$ -> $e$)) >>

    | "(@" e:(expression_lvl (next_exp App)) args:{v:{s:"<"|s:">"} ? a:(expression_lvl (next_exp App))}* ")" ->
       let apply = List.fold_left (fun acc arg -> <:expr<$acc$ $arg$>>) in
       Atom, let rec fn args = match args with
		 [] -> e
	       | (v,arg)::args ->
		 match v with
		 | None ->
		    let res = fn args in
		    <:expr< let $lid:cont$ = fun v -> 
                       let $lid:cont$ = fun f -> (f v $lid:cont$)
		       in $res$
                     in $arg$ >> 
		 | Some '<' ->
		    let res = fn args in
		    <:expr< let v = $arg$ in
                       let $lid:cont$ = fun f -> f v $lid:cont$
		       in $res$
                     >> 
		 | Some '>' ->
		    let res = fn args in
		    <:expr< $res$ $arg$ $lid:cont$ >>
	         | _ -> assert false in fn (List.rev args)
				  
    | (key_word "mu") id:lowercase_ident STR("->") e:(expression_lvl Let) ->
       Let, <:expr< let $lid:id$ = $lid:cont$ in $e$ >>

	      				 
    | "[" id:lowercase_ident "]" e:(expression_lvl App) ->
       App, <:expr< let $lid:cont$ : 'a -> empty = $lid:id$ in $e$ >>

    | (key_word "return") e:(expression_lvl (next_exp App)) ->
       App, <:expr< ($lid:cont$ $e$)>>

    let extra_expressions = extension::extra_expressions
    let _ = add_reserved_id "mu"
    let _ = add_reserved_id "return"

end
module M = Pa_main.Start(Pa_ocaml.Make(Ext(Initial)))
