open Fmlib
open Common


module Pi_info = Term.Pi_info

module Lambda_info = Term.Lambda_info


let builtin_functions: Term.Value.t String_map.t =
    let open String_map in
    empty
    |> add "int_plus"      Term.Value.int_plus
    |> add "int_minus"     Term.Value.int_minus
    |> add "int_times"     Term.Value.int_times
    |> add "int_negate"    Term.Value.int_negate
    |> add "string_concat" Term.Value.string_concat

let _ =
    String_map.mem "int_plus" builtin_functions


type definition =
  | Axiom
  | Assumption
  | Builtin_type of string
  | Builtin of string * Term.Value.t
  | Definition of Term.t
  | Inductive_type of int * int
  | Constructor of int * int * int


type entry = {
    name: string;
    typ: Term.typ;
    definition: definition
  }


type t = {
        entries: entry Sequence.t;
        inductives: (int * Inductive.t) Sequence.t;
        builtin_types: int String_map.t;
}


let empty: t =
    {
        entries = Sequence.empty;
        inductives = Sequence.empty;
        builtin_types = String_map.empty;
    }



let bruijn_convert (i:int) (n:int): int =
  n - i - 1



let count (c:t): int =
    Sequence.length c.entries


let count_inductive (c: t): int =
    Sequence.length c.inductives


let is_valid_index (i:int) (c:t): bool =
  0 <= i && i < count c


let index_of_level (i:int) (c:t): int =
  bruijn_convert i (count c)


let level_of_index (i:int) (c:t): int =
  bruijn_convert i (count c)


let level_forall (p: int -> bool) (term: Term.t) (c: t): bool =
    Term.forall
        (fun level -> p (index_of_level level c))
        term


let level_has (p: int -> bool) (term: Term.t) (c: t): bool =
    Term.has
        (fun level -> p (index_of_level level c))
        term



let entry (level: int) (c: t): entry =
  assert (level < count c);
  Sequence.elem level c.entries


let raw_type_at_level (i:int) (c:t): Term.typ =
  (entry i c).typ


let type_at_level (i:int) (c:t): Term.typ =
  let cnt = count c in
  Term.up (cnt - i) (entry i c).typ



let variable_at_level (i:int) (c:t): Term.t =
    Term.Variable (index_of_level i c)



let name_at_level (level: int) (gamma: t): string =
    (entry level gamma).name


let name_of_index (i: int) (gamma: t): string =
    (entry (bruijn_convert i (count gamma)) gamma).name



let push (name: string) (typ: Term.typ) (definition: definition) (c: t): t =
    {c with
        entries =
            Sequence.push
                {name; typ; definition}
                c.entries;
    }


let push_local (nme: string) (typ: Term.typ) (c:t): t =
    push nme typ Assumption c


let add_definition
    (name: string) (typ: Term.typ) (def: Term.t) (c: t)
    : t
=
    push name typ (Definition def) c





let add_axiom (name: string) (typ: Term.typ) (c: t): t =
    push
        name
        typ
        Axiom
        c



let add_builtin_type (descr: string) (name: string) (typ: Term.typ) (c: t): t =
    let cnt = count c in
    push
        name
        typ
        (Builtin_type descr)
        {c with
            builtin_types =
                String_map.add descr cnt c.builtin_types}



let add_builtin_function
    (descr: string) (name: string) (typ: Term.typ) (c: t): t
=
    let value = String_map.find descr builtin_functions
    in
    push
        name
        typ
        (Builtin (descr, value))
        c



let add_inductive (ind: Inductive.t) (c: t): t =
    let cnti0 = count_inductive c
    and cnt0  = count c
    and ntypes = Inductive.count_types ind
    in
    let open Common.Interval in
    let c1 =
        fold
            c
            (fun i ->
                let name, typ = Inductive.ith_type i ind in
                push
                    name
                    (Term.up i typ)
                    (Inductive_type (cnti0, i))
            )
            0 ntypes
    in
    let c2 =
        fold
            c1
            (fun i c ->
                let nprevious =
                    Inductive.count_previous_constructors i ind
                in
                fold
                    c
                    (fun j ->
                        let name, typ =
                            Inductive.constructor i j ind
                        in
                        push
                            name
                            (Term.up (nprevious + j) typ)
                            (Constructor (cnti0, i, j))
                    )
                    0 (Inductive.count_constructors i ind)
            )
            0 ntypes
    in
    { c2 with
        inductives =
            Sequence.push (cnt0, ind) c.inductives;
    }


let inductive_at_level (level: int) (c: t): Inductive.t option =
    match (Sequence.elem level c.entries).definition with
    | Inductive_type (i, _) ->
       let cnt0, ind = Sequence.elem i c.inductives in
        Some (Inductive.up (count c - cnt0) ind)

    | _ ->
        None





let int_type (c:t) =
    Term.Variable (
        index_of_level
            (String_map.find "int_type" c.builtin_types)
            c
    )


let char_type (c:t) =
    Term.Variable (
        index_of_level
            (String_map.find "character_type" c.builtin_types)
            c
    )


let string_type (c:t) =
    Term.Variable (
        index_of_level
            (String_map.find "string_type" c.builtin_types)
            c
    )


let type_of_literal (v: Term.Value.t) (c: t): Term.typ =
  let open Term in
  match v with
  | Value.Int _ ->
      int_type c

  | Value.Char _ ->
      char_type c

  | Value.String _ ->
      string_type c

  | Value.Unary _ | Value.Binary _ ->
      assert false (* Illegal call! *)




let type_of_variable (i: int) (c: t): Term.typ =
  type_at_level (level_of_index i c) c




let definition_term (idx: int) (c: t): Term.t option =
    let level = level_of_index idx c
    in
    match
      (entry level c).definition
    with
    | Definition def ->
       Some (Term.up (count c - level) def)

    | _ ->
       None



let compute (t:Term.t) (c:t): Term.t =
  let open Term in
  let rec compute term steps c =
    match term with
    | Sort _ | Value _ ->
        term, steps

    | Variable i ->
        let level = level_of_index i c in
        (
            match (entry level c).definition with
            | Axiom | Assumption | Builtin_type _ ->
                term, steps

            | Builtin (_, v) ->
               Term.Value v, steps + 1

            | Definition def ->
               Term.up (count c - level) def,
               steps + 1

            | Inductive_type _ | Constructor _ ->
                term, steps
        )

    | Typed (e, _ ) ->
       compute e (steps + 1) c

    | Appl (Value f, Value arg, _) ->
        Value (Value.apply f arg), steps + 1

    | Appl (Value f, arg, mode) ->
        let arg, new_steps = compute arg steps c in
        if steps < new_steps then
          compute (Appl (Value f, arg, mode)) new_steps c
        else
          Appl (Value f, arg, mode), steps

    | Appl (Lambda (_, exp, _), arg, _) ->
        compute (apply exp arg) (steps + 1) c

    | Appl (Variable i, arg, mode) ->
      let f, new_steps = compute (Variable i) steps c in
      if steps < new_steps then
        compute (Appl (f, arg, mode)) new_steps c
      else
        term, new_steps

    | Appl (f, arg, mode) ->
        let f, new_steps = compute f steps c in
        if steps < new_steps then
          compute (Appl (f, arg, mode)) new_steps c
        else
          term, new_steps

    | Lambda _ ->
        term, steps

    | Pi (arg_tp, res_tp, info) ->
        let c_inner = push_local (Pi_info.name info) arg_tp c in
        let res_tp, new_steps = compute res_tp steps c_inner in
        if steps < new_steps then
            compute (Pi (arg_tp, res_tp, info)) new_steps c
        else
            term, steps

    | Where (_, _, exp, def) ->
        compute (apply exp def) (steps + 1) c
  in
  fst (compute t 0 c)
