open Cairo
open Diagram
open UtilsLib
open Grammars.Environment
open Logic.Abstract_syntax
open Logic.Lambda.Lambda

open Show_exts


module Lambda_show (T : Show_text_sig) = struct

  open T

  let rec fix (f : ('a -> 'b) -> ('a -> 'b)) : 'a -> 'b =
    fun x -> f (fix f) x

  let parenthesize_d ((d, b) : diagram * bool) : diagram =
    match b with
    | true -> d
    | false -> hcat [n "(";
                     d;
                     n ")" ]

  let term_to_diagram_open
      (recur_fn : term -> int -> int -> env * env -> consts -> diagram * bool)
      (t : term)
      (l_level : int)
      (level : int)
      ((l_env, env) : env * env)
      (id_to_sym : consts)
      : diagram * bool =
    let recurse t l_level level (l_env,env) =
      recur_fn t l_level level (l_env,env) id_to_sym in
    let d, b = match t with
    | Var i -> n @@ List.assoc (level - 1 - i) env, true
    | LVar i -> n @@ List.assoc (l_level - 1 - i) l_env, true
    | Const id | DConst id -> n @@ snd @@ id_to_sym id, true
    | Abs (x, t) ->
        let x' = generate_var_name x (l_env, env) in
        let vars,l,u = unfold_abs [(level, x')] (level + 1)
                                  (l_env, (level, x') :: env) t in
          hcat [ n "λ";
                 n @@ Utils.string_of_list " " snd @@ List.rev vars;
                 n ". ";
                 fst @@ recurse u l_level l (l_env, vars @ env) ],
          false
    | LAbs (x, t) ->
        let x' = generate_var_name x (l_env, env) in
        let vars,l,u = unfold_labs [(l_level, x')] (l_level + 1)
                                   ((l_level, x') :: l_env, env) t in
          hcat [ n "λᵒ";
                 n @@ Utils.string_of_list " " snd @@ List.rev vars;
                 n ". ";
                 fst @@ recurse u l level (vars @ l_env, env) ],
          false
    | App ((Const id | DConst id) as binder, Abs(x, u))
      when is_binder id id_to_sym ->
        let x' = generate_var_name x (l_env, env) in
        let vars,l_l,l,u = unfold_binder id l_level (level + 1) id_to_sym
                                         [level,(x',Abstract_syntax.Non_linear)]
                                         (l_env, (level, x') :: env) u in
        let new_env = List.fold_right
                        (fun (l,(x,abs)) (l_acc, acc) ->
                           match abs with
                           | Abstract_syntax.Non_linear -> l_acc, (l, x) :: acc
                           | Abstract_syntax.Linear -> (l, x) :: l_acc, acc)
                        vars (l_env, env) in
          hcat [ parenthesize_d @@ recurse binder l_l l new_env;
                 n " ";
                 n @@ Utils.string_of_list " " (snd >> fst) @@ List.rev vars;
                 n ". ";
                 fst @@ recurse u l_l l new_env ],
          false
    | App ((Const id | DConst id) as binder, LAbs(x, u))
      when is_binder id id_to_sym ->
        let x' = generate_var_name x (l_env, env) in
        let vars,l_l,l,u = unfold_binder id (l_level + 1) level id_to_sym
                           [l_level,(x',Abstract_syntax.Linear)]
                           ((l_level, x') :: l_env, env) u in
        let new_env = List.fold_right
                        (fun (l,(x,abs)) (l_acc,acc) ->
                           match abs with
                           | Abstract_syntax.Non_linear -> l_acc, (l, x) :: acc
                           | Abstract_syntax.Linear -> (l, x) :: l_acc, acc)
                        vars (l_env, env) in
          hcat [ parenthesize_d @@ recurse binder l_l l new_env;
                 n " ";
                 n @@ Utils.string_of_list " " (snd >> fst) @@ List.rev vars;
                 n ". ";
                 fst @@ recurse u l_l l new_env ],
          false
    | App (App ((Const id | DConst id) as op, t1), t2)
      when is_infix id id_to_sym ->
        hcat [ parenthesize_d @@ recurse t1 l_level level (l_env, env);
               n " ";
               parenthesize_d @@ recurse op l_level level (l_env, env);
               n " ";
               parenthesize_d @@ recurse t2 l_level level (l_env, env) ],
        false
    | App (t1, t2) ->
        let args,fn = unfold_app [t2] t1 in
          hcat @@ [ parenthesize_d @@ recurse fn l_level level (l_env, env);
                    n " "; ]
                  @ Utils.intersperse (n " ") @@
                      List.map (fun x -> parenthesize_d @@
                                  recurse x l_level level (l_env, env)) args,
          false
    | _ -> failwith "Not yet implemented" in
    centerX d, b

  let term_to_diagram (t : term) (id_to_sym : consts) : diagram =
    fst @@ fix term_to_diagram_open t 0 0 ([], []) id_to_sym

end


module Make (E : Environment_sig)
            (T : Show_text_sig)
            (C : Show_colors_sig)
            (Emb : Show_embellish_sig) = struct

  type signature = E.Signature1.t
  type lexicon = E.Lexicon.t
  type term = E.Signature1.term
  type 'a tree = 'a Tree.t

  open T

  module L = Lambda_show(T)
  open L

  let replace_with_dict : (string * string) list -> string -> string =
    List.fold_right (fun (ugly, pretty) ->
                       Str.global_replace (Str.regexp_string ugly) pretty)

  let type_to_diagram (sg : signature) (ty : stype) : diagram =
    type_to_string ty (E.Signature1.id_to_string sg) 
    |> replace_with_dict [ ("->", "⊸");
                           ("=>", "→") ]
    |> n


  let abstract_sig (lex : lexicon) : signature =
    fst @@ E.Lexicon.get_sig lex

  let object_sig (lex : lexicon) : signature =
    snd @@ E.Lexicon.get_sig lex

  let sig_name (sg : signature) : string =
    fst @@ E.Signature1.name sg

  let interpret_term (t : term) (lex : lexicon) : term =
    E.Lexicon.interpret_term t lex
    |> normalize
       ~id_to_term:(fun i ->
                      E.Signature1.unfold_term_definition i @@ object_sig lex)


  let rec term_to_graph (sg : signature) (t : term) : term tree =
    let children =
      match t with
      | Var _ | LVar _ | Const _ | DConst _ ->
          []
      | Abs (_, body) | LAbs (_, body) ->
          [ body ]
      | App (App ((Const id | DConst id) as op, t1), t2)
        when is_infix id (E.Signature1.id_to_string sg) ->
          [ t1; op; t2 ]
      | App (fn, arg) ->
          [ fn; arg ]
      | _ -> failwith "Not yet implemented" in
    Tree.T (t, List.map (term_to_graph sg) children)


  let rec render_term_graph (l_level : int) (level : int)
                            (l_env, env : env * env)
                            (render_term : term -> int -> int -> env * env -> diagram)
                            ((Tree.T (term, children)) : term tree)
                            : diagram tree =

    let render_children_in l_level level (l_env, env) =
      List.map (render_term_graph l_level level (l_env, env) render_term) children in

    let children_d = match term with
      | Abs (x, _) ->
          let x' = generate_var_name x (l_env, env) in
          let env' = (level, x') :: env in
          let level' = level + 1 in
          render_children_in l_level level' (l_env, env')
      | LAbs (x, _) ->
          let x' = generate_var_name x (l_env, env) in
          let l_env' = (l_level, x') :: l_env in
          let l_level' = l_level + 1 in
          render_children_in l_level' level (l_env', env)
      | _ ->
          render_children_in l_level level (l_env, env) in

    Tree.T (render_term term l_level level (l_env, env), children_d)


  let term_to_diagram_in (config: Rendering_config.config) (sg : signature) (t : term)
                         (l_level : int) (level : int)
                         ((l_env, env) : env * env) : diagram =
    let ttd = term_to_diagram_open
	      (*              |> Emb.embellishments (sig_name sg) *)
              |> Emb.embellishments_functions (sig_name sg) config
              |> fix in
    let consts = E.Signature1.id_to_string sg in
    fst @@ ttd t l_level level (l_env, env) consts
 

  let merge_trees : 'a tree list -> 'a list tree =
    List.map (Tree.map (fun x -> [x]))
    >> Utils.fold_left1 (Tree.map2 (@))

  let decorate_lines (lines : diagram list) : diagram list =
    lines
    |> List.map (pad_abs ~horizontal:2.0)
    |> List.map (pad_rel ~vertical:0.05)
    |> List.map2 color @@ Utils.cycle (List.length lines) C.lines

  let rec align_sister_lines (tree : diagram list tree) : diagram list tree =
    match tree with
    | Tree.T (lines, []) -> tree
    | Tree.T (lines, children) ->
        let children = List.map align_sister_lines children in
        let heights = List.map (fun (Tree.T (c_lines, _)) ->
                        List.map (fun c_line -> (extents c_line).h)
                                 c_lines)
                               children in
        let max_heights = Utils.fold_left1 (List.map2 max) heights in
        let children = List.map (fun (Tree.T (c_lines, c_children)) ->
                         Tree.T (List.map2 (fun c_line new_height ->
                                   let height_diff = new_height -.
                                                     (extents c_line).h in
                                   pad_abs ~vertical:(height_diff /. 2.) c_line)
                                           c_lines max_heights,
                                 c_children))
                         children in
        Tree.T (lines, children)


  let realize_diagram (abs_term : term) (lexs : lexicon list) (config : Rendering_config.config) : diagram =

    let abs_sig = abstract_sig @@ List.hd lexs in
    (*    let obj_sigs = List.map object_sig lexs in *)
    (*    let _sigs = abs_sig :: obj_sigs in *)

    let expanded_abs_term = normalize (E.Signature1.expand_term abs_term abs_sig) in
    let abs_terms_differ = abs_term != expanded_abs_term in
    
    let term_graph = term_to_graph abs_sig abs_term in

    let render_abs_term = term_to_diagram_in config abs_sig in

    let render_obj_term lex abs_term =
      let obj_sig = object_sig lex in
      let obj_term = interpret_term abs_term lex in
      term_to_diagram_in config obj_sig obj_term in

    let abs_term_tree = render_term_graph 0 0 ([], [])
                                          render_abs_term
                                          term_graph in
    let last_abs_term_graph =
      if abs_terms_differ then
	term_to_graph abs_sig expanded_abs_term
      else
	term_graph in
    
    let obj_term_trees = List.map (fun lex ->
                                     render_term_graph 0 0 ([], [])
                                                       (render_obj_term lex)
                                                       last_abs_term_graph)
                                  lexs in

    let trees =
      if abs_terms_differ then
	let expanded_abs_term_tree =
	  render_term_graph 0 0 ([], [])
            render_abs_term
		    last_abs_term_graph 
        in
      (*      		abs_term_tree :: (expanded_abs_term_tree :: obj_term_trees)   *)
      (*      	abs_term_tree :: obj_term_trees    *)
             expanded_abs_term_tree :: obj_term_trees   
      else
	abs_term_tree :: obj_term_trees in

    trees
    |> merge_trees
    |> Tree.map decorate_lines
    |> align_sister_lines
    |> Tree.map (List.map centerX >> vcat)
    |> Tree.map (bg_color (C.node_background config))
    |> Tree.to_diagram
    |> setup (fun cr -> set_line_width cr 1.5)
    |> bg_color (C.background config)
    |> color C.tree

end
