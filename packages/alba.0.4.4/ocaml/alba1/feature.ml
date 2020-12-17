open Term
open Container

module Option = Fmlib.Option

module Spec = struct
  type t = {nms:   int array;
            def:   term option;
            pres:  term list;
            posts: term list}

  let make_empty (nms:int array): t =
    {nms = nms; def = None; pres = []; posts = []}

  let make_func_def (nms:int array) (def: term option) (pres:term list): t =
    {nms = nms; def = def; pres = pres; posts = []}


  let make_func_spec (nms:int array) (pres: term list) (posts: term list): t =
    {nms = nms; def = None; pres = pres; posts = posts}

  let names (spec:t): int * int array =
    Array.length spec.nms, spec.nms

  let is_empty (s:t): bool =
    s.pres = [] && s.def = None && s.posts = []

  let count_arguments (spec:t): int =
    Array.length spec.nms

  let has_definition_term (spec:t): bool =
    Option.has spec.def

  let definition_term_opt (spec:t): term option =
    spec.def

  let definition_term (spec:t): term =
    match spec.def with
      None   -> raise Not_found
    | Some t -> t

  let has_preconditions (spec:t): bool =
    spec.pres <> []

  let preconditions (spec:t): term list =
    spec.pres


  let count_postconditions (spec:t): int =
    List.length spec.posts


  let postcondition (i:int) (spec:t): term =
    assert (i < count_postconditions spec);
    List.nth spec.posts i


  let has_postconditions (spec:t): bool =
    spec.posts <> []


  let postconditions (spec:t): term list =
    spec.posts

  let has_no_definition (s:t): bool =
    not (has_definition_term s || has_postconditions s)

  let without_definition (s:t): t =
    {s with def = None; posts = []}


  let equivalent (s1:t) (s2:t): bool =
    (* equivalent ignoring names *)
    if s1.def <> s2.def && Option.has s1.def && Option.has s2.def then
      Printf.printf "definitions not equivalent %s %s\n"
        (Term.to_string (Option.value s1.def))
        (Term.to_string (Option.value s2.def));
    s1.def = s2.def && s1.pres = s2.pres && s1.posts = s2.posts


  let is_consistent (s:t) (snew:t): bool =
    Term.equivalent_list s.pres snew.pres
      &&
    (snew.posts = [] || Term.equivalent_list snew.posts s.posts)
      &&
    match snew.def, s.def with
      None, _ ->
        true
    | Some snewdef, Some sdef ->
        Term.equivalent snewdef sdef
    | _ ->
       false

end


type implementation =
    Builtin
  | Deferred
  | Empty
  (*| Code of ???*)

type body = Spec.t * implementation
