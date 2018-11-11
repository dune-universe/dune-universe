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
    (fun name ->
     let c = !cont in
     cont := c+1;
     name ^ string_of_int c)

  let app _loc e args =
	      if args = [] then e else
	      (fun k ->
			  let args = List.map (fun a -> a, gen_sym "a") args in
			  let start = 
			       let name = gen_sym "f" in
			       let r = List.fold_left (fun acc (arg,name) -> <:expr< $acc$ $lid:name$>>) <:expr< $lid:name$ >> args in
			       e <:expr<fun $lid:name$ -> $r$ $k$>>
			  in
			  let res = List.fold_left (fun acc (arg,name) ->
						    arg <:expr< fun $lid:name$ -> $acc$ >>) start args
			  in
			  res)

  let stack = Stack.create ()
			   
  let cexpr = Decap.declare_grammar "cexpr"
  let _ = Decap.set_grammar cexpr
	    (parser
	    | '$' - id:lowercase_ident -> (fun k -> <:expr<$lid:id$ $k$>>)
	    | "(@" e:cexpr args:cexpr* "@)" -> app _loc e args
	    | (key_word "fun") id:lowercase_ident STR("->") e:cexpr ->
	       (fun k ->
			    let k' = gen_sym "k" in
  		        <:expr< $k$ (fun $lid:id$ $lid:k'$ -> $e <:expr< $lid:k'$ >>$) >>)
 	  | (key_word "mu") id:lowercase_ident STR("->") e:cexpr ->
              (fun k -> <:expr< let $lid:id$ = $k$ in $e k$ >>)
	  | "(*" e:cexpr id:lowercase_ident "*)" ->
	      (fun k ->  <:expr< $e <:expr< $lid:id$ >>$ >>)
	  | "(" e:cexpr ")" -> e
	  | (key_word "return") e:(expression_lvl Top) -> (fun k -> <:expr< $k$ $e$>>)			 
	  | "(-" ->> let k' = gen_sym "k" in let _ = Printf.eprintf "Push\n%!"; Stack.push k' stack in DEBUG"0" e:(expression_lvl Top) "-)" DEBUG"1" ->
			(let res k =  <:expr< let $lid:k'$ = $k$ in $e$>> in (try ignore (Printf.eprintf "Pop\n%!"; Stack.pop stack) with Stack.Empty -> ()); res))
		   
    let extension = parser
		  | "(@" e:cexpr args:cexpr* "@)" ->
				      Atom, (try let k = Stack.top stack in Printf.eprintf "Non empty\n%!"; app _loc e args <:expr<$lid:k$>>
				      with Stack.Empty ->
					Printf.eprintf "Empty\n%!";
					let k = gen_sym "k" in
					 <:expr< fun $lid:k$ -> 
					       $app _loc e args <:expr<$lid:k$>>$>>)

    let extra_expressions = extension::extra_expressions
    let _ = add_reserved_id "mu"
    let _ = add_reserved_id "return"

end
module M = Pa_main.Start(Pa_ocaml.Make(Ext(Initial)))
