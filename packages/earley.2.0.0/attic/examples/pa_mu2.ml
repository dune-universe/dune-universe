open Pa_ocaml_prelude
  
#define LOCATE locate
  
module Ext = functor(In:Extension) -> 
struct
  include In

  type 'a term =
      Term of 'a
    | Context of 'a -> 'a

  let gen_sym =
    let cont = ref 0 in
    (fun () ->
     let c = !cont in
     cont := c+1;
     "_v_pa_mu" ^ string_of_int c)

        let mk_cont _loc t u = match t with
      Term t -> <:expr< $t$ $u$ >>
    | Context t -> t u

  let to_term _loc t = match t with
      Term t -> t
    | Context t ->
       let id = gen_sym () in
       <:expr< fun $lid:id$ -> $t <:expr<$lid:id$>>$ >>

  let extension = parser
    | (key_word "cfun") id:lowercase_ident STR("->") e:(expression_lvl Let) ->
      Let, <:expr< fun _k_pa_mu ->  _k_pa_mu (fun $lid:id$ -> $e$) >>
    | "(@" e:(expression_lvl (next_exp App)) args:(expression_lvl (next_exp App))* ")" ->
       Atom, to_term _loc (List.fold_left (
			       fun acc arg ->
			       let id = gen_sym () in
			       Context (fun _k1_pa_mu -> <:expr< $arg$ (fun $lid:id$ -> $mk_cont _loc acc <:expr< fun _k_pa_mu -> _k_pa_mu $lid:id$ $_k1_pa_mu$ >>$)>>))
			    (Term e) args)
    | "(*" e:(expression_lvl (next_exp App)) args:(expression_lvl (next_exp App))* ")" ->
       Atom, to_term _loc (List.fold_left (
			       fun acc arg ->
			       let id = gen_sym () in
			       Context (fun _k1_pa_mu -> <:expr< $arg$ (fun $lid:id$ -> $mk_cont _loc acc <:expr< fun _k_pa_mu -> _k_pa_mu $lid:id$ $_k1_pa_mu$ >>$)>>))
			    (Term e) args)
    | (key_word "mu") id:lowercase_ident STR("->") e:(expression_lvl Let) ->
       Let, <:expr< fun $lid:id$ -> $e$ $lid:id$ >>

    | (key_word "return") e:(expression_lvl (next_exp App)) ->
       App, <:expr<fun k -> k $e$>>

    | (key_word "send") e:(expression_lvl (next_exp App)) e':(expression_lvl (next_exp App)) ->
       App, <:expr<fun k -> $e$ (fun k' -> k' $e'$ k)>>
	 
    | (key_word "receive") e:(expression_lvl (next_exp App)) e':(expression_lvl (next_exp App)) ->
       App, <:expr<fun k -> $e'$ (fun v -> k ($e$ v))>>

    | (key_word "receive2") e:(expression_lvl (next_exp App)) e':(expression_lvl (next_exp App)) e'':(expression_lvl (next_exp App)) ->
       App, <:expr<fun k -> $e''$ (fun v2 -> $e'$ (fun v1 -> k ($e$ v1 v2)))>>
	 
     | "(*" e:(expression_lvl (next_exp App)) e':(expression_lvl (next_exp App))   ")" ->
        Atom, <:expr< fun _k_pa_mu -> $e$ $e'$ >>

    let extra_expressions = extension::extra_expressions
    let _ = add_reserved_id "cfun"
    let _ = add_reserved_id "mu"
    let _ = add_reserved_id "return"
    let _ = add_reserved_id "send"

end
module M = Pa_main.Start(Pa_ocaml.Make(Ext(Initial)))
