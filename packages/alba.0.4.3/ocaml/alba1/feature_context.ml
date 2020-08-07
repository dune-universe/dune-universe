open Term

type t = {
    stack: Formals.t list;
    cc: Class_context.t;
    ft: Feature_table.t
  }

let make (ft:Feature_table.t): t =
  {stack = [];
   cc = Class_context.make (Feature_table.class_table ft);
   ft}

let make_from_arguments
      (nms:names) (tps:types) (fgnms:names) (fgtps:types) (ft:Feature_table.t)
    : t =
  {stack = [Formals.make nms tps];
   cc = Class_context.make_from_fgs fgnms fgtps (Feature_table.class_table ft);
   ft}

let is_global (fc:t): bool =
  fc.stack = []

let is_local (fc:t): bool =
  fc.stack <> []

let pop (fc:t): t =
  assert (is_local fc);
  {fc with
    stack = List.tl fc.stack;
    cc = Class_context.pop fc.cc}


let push (nms:names) (tps:types) (fgnms:names) (fgtps:types) (fc:t): t =
  let nfgs = Array.length fgnms in
  {fc with
    stack =
      begin
        match fc.stack with
        | [] ->
           [Formals.make nms tps]
        | formals :: _ ->
           Formals.make
             (Array.append nms (Formals.names formals))
             (Array.append
                tps
                (Array.map (Term.up nfgs) (Formals.types formals)))
          :: fc.stack
      end;
    cc = Class_context.push fgnms fgtps fc.cc}


let formals (fc:t): Formals.t =
  match fc.stack with
  | [] ->
     Formals.empty
  | fs :: _ ->
     fs

let count_variables (fc:t): int =
  Formals.count (formals fc)


let variable_type (i:int) (fc:t): type_term =
  let fs = formals fc in
  assert (i < Formals.count fs);
  (Formals.types fs).(i)


let tvars (fc:t): Tvars.t =
  Class_context.tvars fc.cc

let class_context (fc:t): Class_context.t =
  fc.cc

let class_table (fc:t): Class_table.t =
  Class_context.class_table fc.cc

let feature_table (fc:t): Feature_table.t =
  fc.ft

let string_of_term (t:term) (fc:t): string =
  Feature_table.term_to_string
    t true true 0
    (Formals.names (formals fc))
    (Class_context.tvars fc.cc)
    fc.ft


let string_of_signature (i:int) (fc:t): string =
  let nvars = count_variables fc in
  assert (nvars <= i);
  Feature_table.string_of_signature (i-nvars) fc.ft


let split_general_implication_chain
      (t:term) (fc:t)
    : int * formals * formals * term list * term =
  Term.split_general_implication_chain
    t
    (Constants.implication_index + count_variables fc)
