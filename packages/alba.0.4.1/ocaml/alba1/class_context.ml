open Term

type t = {
    stack: Tvars.t list;
    ct:Class_table.t
  }

let make (ct:Class_table.t): t =
  {stack = []; ct}

let make_from_tvs (tvs:Tvars.t) (ct:Class_table.t): t =
  {stack = [tvs];
   ct}

let make_from_fgs (fgnms:names) (fgtps:types) (ct:Class_table.t): t =
  {stack = [Tvars.make_fgs fgnms fgtps];
   ct}

let is_global (cc:t): bool =
  cc.stack = []

let is_local (cc:t): bool =
  cc.stack <> []

let pop (cc:t): t =
  assert (is_local cc);
  {cc with stack = List.tl cc.stack}

let push (fgnms:names) (fgcon:types) (cc:t): t =
  {cc with
    stack =
      match cc.stack with
      | [] ->
         [Tvars.make_fgs fgnms fgcon]
      | tvs::_ ->
         Tvars.push_fgs fgnms fgcon tvs :: cc.stack}


let tvars (cc:t): Tvars.t =
  match cc.stack with
  | [] ->
     Tvars.empty
  | tvs :: _ ->
     tvs

let class_table (cc:t): Class_table.t =
  cc.ct
    
let string_of_type (tp:type_term) (cc:t): string =
  Class_table.string_of_type tp (tvars cc) cc.ct
