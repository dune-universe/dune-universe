open Cairo
open Diagram
open UtilsLib
open Logic.Lambda.Lambda
open Show_exts


module Make (T : Show_text_sig) : Show_embellish_sig = struct

  open T

  module L = Show.Lambda_show(T)
  open L

  let rec unfold_cats (cat_id : int) (t : term) : term list =
    match t with
    | App(App((Const id | DConst id), t1), t2)
      when id = cat_id ->
        unfold_cats cat_id t1 @ unfold_cats cat_id t2
    | _ -> [ t ]

  let simplify_cats default_fn recur_fn t l_level level (l_env, env) id_to_sym =
    let recurse t l_level level (l_env, env) =
      recur_fn t l_level level (l_env, env) id_to_sym in
    match t with
    | App(App((Const id | DConst id) as op, t1), t2)
      when is_infix id id_to_sym && ("+" = snd @@ id_to_sym id)->
        let args = unfold_cats id t1 @ unfold_cats id t2 in
        let sep = hcat [ n " ";
                         parenthesize_d @@ recurse op l_level level (l_env, env);
                         n " " ] in
          (args |> List.map (fun arg -> parenthesize_d @@
                               recurse arg l_level level (l_env, env))
                |> Utils.intersperse sep
                |> hcat), false
    | _ -> default_fn recur_fn t l_level level (l_env, env) id_to_sym

  let render_constants_with render_fn default_fn recur_fn t l_level level (l_env, env) id_to_sym =
    match t with
    | Const id | DConst id -> render_fn @@ snd @@ id_to_sym id, true
    | _ -> default_fn recur_fn t l_level level (l_env, env) id_to_sym

  let logic_const name =
    let name = match name with
               | "Ex" -> "∃"
               | "ExUni" -> "∃!"
               | "Ex_l" -> "∃ₗ"
	       | "Ex_t" -> "∃ₜ"
               | "All" -> "∀"
	       | "All_t" -> "∀ₜ"
	       | "TOP" -> "⊤"
               | "The" -> "ι"
               | "&" -> "∧"
               | ">" -> "⇒"
	       | "~" -> "¬"
               | _ -> name in
    match name with
    | "∃!"
    | "∃ₗ"
    | "∃ₜ"
    | "∃"
    | "∀"
    | "∀ₜ"
    | "ι" -> n name
             |> reframe (fun exts ->
                           { exts with w = exts.w -. (extents (n " ")).w })
    | "∧"
    | "⇒" -> n name
    | _ -> b name


  let symbol_with_arity s =
    let regexp = Str.regexp "^\\([A-Za-z]+\\)[0-9]+$" in
    if Str.string_match regexp s 0 then
      let s' = Str.matched_string s in
      Str.matched_group 1 s'
    else
      s
    
  let tag_derived_tree_cst s = symbol_with_arity s
	     
  let string_const = function
    | "e" -> n "ε"
    | name -> i name

  let render_with_color c default_fn recur_fn t l_level level (l_env, env) id_to_sym =
    let d, b = default_fn recur_fn t l_level level (l_env, env) id_to_sym in
    color c d, b
    [@@warning "-32"]

  let big_parens (d : diagram) : diagram =
    let paren_height = (extents @@ tighten_text @@ n "(").h in
    let d_height = (extents d).h in
    let y_scale = d_height /. paren_height in
    let x_scale = y_scale ** 0.125 in
    let scale_paren p =
      n p |> tighten_text
          |> centerY
          |> scale (x_scale, y_scale)
          |> pad_abs ~left:2. in
    hcat [ scale_paren "(";
           d;
           scale_paren ")"; ]
  
  let tag_style default_fn recur_fn t l_level level (l_env, env) id_to_sym =
    let recurse t l_level level (l_env, env) =
      recur_fn t l_level level (l_env, env) id_to_sym in
    match t with
    | App(t1, t2) ->
        let args,fn = unfold_app [t2] t1 in
        let arg_diagrams = List.map (fun x -> fst @@
                                       recurse x l_level level (l_env, env))
                                    args in
          (match fn with
          | Const _ | DConst _ ->
              Tree.T (parenthesize_d @@
                        recurse fn l_level level (l_env, env),
                      List.map Tree.singleton arg_diagrams)
              |> Tree.to_diagram ~vgap:10.
              |> centerY
              |> setup (fun cr -> set_line_width cr 1.),
              true
          | Var _ | LVar _ ->
              hcat [ parenthesize_d @@ recurse fn l_level level (l_env, env);
                     big_parens @@ hcat @@ Utils.intersperse (n ", ") arg_diagrams; ]
              |> centerX,
              false
          | _ -> default_fn recur_fn t l_level level (l_env, env) id_to_sym)
    | _ -> default_fn recur_fn t l_level level (l_env, env) id_to_sym
 


  let embellishments = function
    | "Strings"
    | "strings"
    | "anglais"
    | "francais" -> simplify_cats >> render_constants_with string_const
    | "labelled_logic"
    | "logic"
    | "logique"
    | "HybridLogic"
    | "semantics" -> render_constants_with logic_const
    | "Trees"
    | "Derived_trees"
    | "trees"
    | "derived_trees" -> fun x y z t u v w -> tag_style x y z t u v (fun x ->  let b,s = w x in b, tag_derived_tree_cst s)
    | "discourse_grammar"
    | "Derivations"
    | "derivations"
    | "Derivation_trees"
    | "derivation_trees"
    | "TAG"
    | "DSTAG" -> fun x y z t u v w -> tag_style x y z t u v (fun x ->  let b,s = w x in b, s)
    | _ -> fun x -> x


  let embellishments_engines = function
    | Rendering_config.STRINGS -> simplify_cats >> render_constants_with string_const
    | Rendering_config.LOGIC -> render_constants_with logic_const
    | Rendering_config.DERIVED_TREES -> fun x y z t u v w -> tag_style x y z t u v (fun x ->  let b,s = w x in b, tag_derived_tree_cst s)
    | Rendering_config.TREES -> fun x y z t u v w -> tag_style x y z t u v (fun x ->  let b,s = w x in b, s)
    | Rendering_config.DEFAULT -> (fun x -> x)
      
  let embellishments_functions sig_name config =
    try
      embellishments_engines (Utils.StringMap.find sig_name (Rendering_config.engines config))
    with
    | Not_found -> (fun x -> x)
    
      
end
