open Idds
open Base

module Pattern = struct
  type t = (Var.t * bool) list
    [@@deriving sexp, compare, hash]
end

module Action = struct
  module T = struct
    type t = (Var.t * bool) list
      [@@deriving sexp, compare, hash]
  end
  include T
  include Comparator.Make(T)
end

type row = Pattern.t * Set.M(Action).t

type t = row list

type tbl = (Pattern.t, Set.M(Action).t) Hashtbl.t

let compare_vars v v' =
  match Var.closer_to_root v v' with
  | Left -> 1
  | Equal -> 0
  | Right -> -1

(** [intersect lst tbl] returns a forwarding table computed from all possible 
    intersections of rows in [lst] *)
let rec intersect (lst:t) (tbl:tbl) =
  let rec build_intersections to_be_intersected rules lst =
    match lst with
    | [] ->
        let intersection = List.concat to_be_intersected in
        (* want to check if two patterns contradict each other e.g. xi=0,xi=1 *)
        if 0 = List.length to_be_intersected then ()
        else if List.contains_dup ~compare:(fun (v,b) (v',b') ->
          let c = compare_vars v v' in
          if 0 = c then Bool.to_int (Bool.equal b b')
          else c
        ) intersection then () 
        else
          let pattern = List.dedup_and_sort ~compare:(fun (v,_) (v',_) ->
            compare_vars v v'
          ) intersection in
          Hashtbl.update tbl pattern ~f:(fun r ->
            match r with
            | None -> rules
            | Some s -> Set.union s rules
          )
    | (p,a)::t -> 
      build_intersections (p::to_be_intersected) (Set.union rules a) t;
      build_intersections to_be_intersected rules t;
  in build_intersections [] (Set.empty (module Action)) lst; 
  List.sort (Hashtbl.to_alist tbl) ~compare:(fun (l, _) (l', _) -> 
    compare (List.length l') (List.length l)
  )


let to_table (idd:Idd.t) = 
  let tbl = Hashtbl.create (module Pattern) in
  let rec to_tables (dd:Dd.t) (pattern:Pattern.t) (rule:Action.t) : unit =
    match dd with
    | True -> 
      let set = (Set.singleton (module Action) rule) in
      Hashtbl.update tbl pattern ~f:(fun r ->
        match r with
        | None -> set
        | Some s -> Set.union s set
      )
    | False -> ()
    | Branch { var; hi; lo } when Var.is_inp var -> 
      (to_tables hi ((var, true)::pattern) rule);
      (to_tables lo ((var, false)::pattern) rule)
    | Branch { var; hi; lo } (* var is output *) ->
      (to_tables hi pattern ((var, true)::rule));
      (to_tables lo pattern ((var, false)::rule))
    in
    (* to_tables (idd :> Dd.t) [] [];
    (Hashtbl.to_alist tbl) *)
    to_tables (idd :> Dd.t) [] [];
    intersect (Hashtbl.to_alist tbl) tbl


let eval (tbl:t) (env:(Var.t -> bool)) : bool =
  let check_lst lst = List.for_all lst ~f:(fun (v, b) -> Bool.equal b (env v)) in
  let rec eval = function
    | (p, a)::t -> if check_lst p then (Set.exists a ~f:check_lst)
                                  else eval t
    | [] -> false
  in
  eval tbl

let rec to_expr (tbl:t) ~interp_test ~interp_act = 
  match tbl with
  | (p,a)::t -> 
    Kat.Optimize.union
      (Kat.Optimize.seq 
        (Kat.Ast.Assert (interp_test p))
        (Set.fold a ~init:(Kat.Ast.Assert (Kat.Ast.False)) ~f:(fun exp elm -> 
            Kat.Optimize.union exp (interp_act elm)
          )
        )
      )
      (to_expr t ~interp_test ~interp_act)
  | [] -> Kat.Ast.Assert (Kat.Ast.False)

(** [to_str_lst p s var_name] is "(var_name x1)[s]b1;...;(var_name xn)[s]bn" if 
    [p]=\[(x1,b1);...;(xn,bn)\] *)
let to_str_lst p s var_name = String.drop_suffix
  (List.fold_right p ~init:"" ~f:(fun (v, b) str ->
      let str1 = Caml.Format.sprintf "%s" (var_name v) in
      let str2 = Caml.Format.sprintf (if b then "1;%s" else "0;%s") str in
      str1 ^ s ^ str2
    )
  )
  1

(** [to_str_act act] is "s1;s2;...;sn" where [act]=\{a1,...,an\} and
    [si] is [to_str_lst ai "←" var_name] *)
let to_str_act var_name =
  Set.fold ~init:"" ~f:(fun str e -> 
    Caml.Format.sprintf "[%s];%s" (to_str_lst e "←" var_name) str
  )
  

(*===========================================================================*)
(* Helper functions for formatting HTML code                                 *)
(*===========================================================================*)
let prelude =
  let l =
    [ "table, th, td { border-collapse: collapse; }"
    ; "table, th, td { font-size: 20px; }"
    ; "table.framed { border: 2px solid black; }"
    ; "table.framed th, table.framed td { border: 1px solid black; }"
    ; "th, td { padding: 3px; }"
    ; "tr:nth-child(even) { background-color: #eee; }"
    ; "tr:nth-child(odd) { background-color: #fff; }"
    ; ".align-center { text-align: center; }"
    ; "th { text-align: center; }"
    ]
  in
  Tyxml.Html.style (List.map ~f:Tyxml.Html.txt l)
  
let prelude_str =
  Caml.Format.asprintf "%a@." (Tyxml.Html.pp_elt ()) prelude

let to_string_doc b =
  let meta_str = "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">" in
  let footer_str =
    "<script> \
     [...document.querySelectorAll('p')].forEach(el => \
       el.addEventListener('click', () => el.nextSibling.style.display = \
       el.nextSibling.style.display === 'none' ? 'block' : 'none')); \
     [...document.querySelectorAll('ul ul')].forEach(el => \
       el.style.display = 'none'); \
     </script>"
  in
  Caml.Format.asprintf "<head>%s%s</head><body>@[%a@]%s</body>@."
    meta_str
    prelude_str
    (Tyxml.Html.pp_elt ())
    (PrintBox_html.to_html b)
    footer_str
(*===========================================================================*)


(** [render_box tbl] is the [PrintBox.t] object representing [tbl] *)
let render_box var_name tbl =
  let matrix = Array.make_matrix ~dimy:2 ~dimx:(1+(List.length tbl)) "" in
  let _ : unit = matrix.(0).(0) <- "Pattern"; matrix.(0).(1) <- "Actions" in
  let _ : unit = List.iteri tbl ~f:(fun i (p,a) ->
    matrix.(i+1).(0) <- (to_str_lst p "=" var_name);
    matrix.(i+1).(1) <- (to_str_act var_name a)
  )
  in
  PrintBox.grid_text matrix

let to_string ?(var_name=Var.to_string) tbl =
  render_box var_name tbl
  |> PrintBox_text.to_string

let render ?(var_name=Var.to_string) tbl =
  let box = render_box var_name tbl in
  let output_file =
    Caml.Filename.(temp_file ("Forwarding Table_") (".html"))
    |> String.tr ~target:' ' ~replacement:'-'
  in
  let out = Stdio.Out_channel.create output_file in
  Stdio.Out_channel.output_string out (to_string_doc box);
  Stdio.Out_channel.close out;
  ignore (Open.in_default_app output_file : bool)
