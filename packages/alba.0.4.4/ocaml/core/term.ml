open Fmlib
open Module_types


let bruijn_convert (i:int) (n:int): int =
  assert (i < n);
  n - i - 1



module Lambda_info =
  struct
    type t = {
        name:  string;    (* name of the argument *)
        typed: bool;      (* false: user has not given type information for
                             the argument '\ x: RT := exp' *)
      }
    let name (i:t): string =
      i.name

    let is_anonymous (i:t): bool =
      i.name = "_"

    let is_typed (i:t): bool =
      i.typed

    let typed (name: string): t =
      {name; typed = true}

    let untyped (name: string): t =
      {name; typed = false}
  end






module Pi_info =
struct
    type t = {
        name:  string; (* name of the argument *)
        typed: bool;   (* false, if user has given no type information 'all x:
                          R' *)
        arrow: bool;   (* user has written 'A -> B' instead of 'all (nme:A):
                          R' or name does not occur in [R] *)
        kind: bool;    (* Argument type is a kind *)
      }


    let make name typed arrow kind = {name; typed; arrow; kind}

    let name (info: t): string =
        info.name

    let is_anonymous (i:t): bool =
      i.name = "_"

    let is_arrow (i:t): bool =
      i.arrow

    let is_typed (i:t): bool =
      i.typed

    let is_implicit (info: t): bool =
        info.kind && not info.arrow


    let arrow: t =
        {name = "_"; arrow = true; typed = true; kind = false}

    let typed (name: string) : t =
        {name; arrow = false; typed = true; kind = false}

    let untyped (name: string) : t =
        {name; arrow = false; typed = false; kind = false}
end





module Application_info =
struct
    type t =
      | Normal
      | Implicit
      | Binary
      | Unary
end








(* ----------------------------------------------------------- *)
(* Term                                                        *)
(* ----------------------------------------------------------- *)


type t =
  | Sort of Sort.t

  | Value of Value.t

  | Variable of int

  | Typed of t * typ

  | Appl of t * t * Application_info.t

  | Lambda of typ * t * Lambda_info.t

  | Pi of typ * typ * Pi_info.t

  | Where of string * typ * t * t

and typ = t

and formal_argument = string * typ

and inductive = {
    base_count: int;
    n_up: int;
    parameters: formal_argument list;
    types: (formal_argument * formal_argument array) array}


type t_n   = t * int
type typ_n = typ * int

let proposition: t =
    Sort Sort.Proposition

let any: t =
    Sort (Sort.Any 0)

let any_uni (uni: int): t =
    Sort (Sort.Any uni)

let char (code:int): t =
    Value (Value.Char code)


let string (s:string): t =
    Value (Value.String s)


let number_values (s:string): t list =
  List.map (fun v -> Value v) (Value.number_values s)


let variable i: t =
    Variable i



let application (f: t) (a: t): t =
    Appl (f, a, Application_info.Normal)


let implicit_application (f: t) (a: t): t =
    Appl (f, a, Application_info.Implicit)


let binary (a: t) (op: t) (b: t): t =
    let mode = Application_info.Binary
    in
    Appl ( Appl (op, a, mode), b, mode)


let rec applications (f: t) (args: t list): t =
    match args with
    | [] ->
        f
    | arg :: args ->
        applications
            (Appl (f, arg, Application_info.Normal))
            args



let lambda0 (name: string) (typed: bool) (tp: typ) (exp: t): t =
    assert (name <> "");
    let info =
        if typed then
            Lambda_info.typed name
        else
            Lambda_info.untyped name
    in
    Lambda (tp, exp, info)


let lambda (name: string) (tp: typ) (exp: t): t =
    lambda0 name true tp exp


let lambda_untyped (name: string) (tp: typ) (exp: t): t =
    lambda0 name false tp exp



let product0 (name: string) (typed: bool) (arg_tp: typ) (result_tp: typ): t =
    assert (name <> "");
    let info =
        if name = "_" then
            Pi_info.arrow
        else if typed then
            Pi_info.typed name
        else
            Pi_info.untyped name
    in
    Pi (arg_tp, result_tp, info)


let product (name: string) (arg_tp: typ) (result_tp: typ): t =
    assert (name <> "");
    let info =
        if name = "_" then
            Pi_info.arrow
        else
            Pi_info.typed name
    in
    Pi (arg_tp, result_tp, info)


let product_untyped (name: string) (arg_tp: typ) (result_tp: typ): t =
    assert (name <> "");
    assert (name <> "_");
    Pi (arg_tp, result_tp, Pi_info.untyped name)


let arrow (arg_tp: typ) (result_tp: typ): t =
    product "_" arg_tp result_tp


let lambda_in (fargs: formal_argument list) (exp: t): t =
    List.fold_right
        (fun (name, arg_tp) ->
            lambda name arg_tp)
        fargs
        exp


let product_in (fargs: formal_argument list) (result_tp: t): t =
    List.fold_right
        (fun (name, arg_tp) ->
            product name arg_tp)
        fargs
        result_tp


let expand_where (name: string) (tp: typ) (exp: t) (def: t): t =
    Appl (
        Lambda (tp, exp, Lambda_info.typed name),
        def,
        Application_info.Normal
    )


let type_of_sort (s: Sort.t): typ =
    Sort (Sort.type_of s)


let is_sort (typ: typ): bool =
    match typ with
    | Sort _ ->
        true
    | _ ->
        false

let pi_sort (arg: typ) (res: typ): typ =
    match arg, res with
    | Sort argsort, Sort ressort ->
        Sort (Sort.pi_sort argsort ressort)
    | _ ->
        assert false (* Illegal call! *)




module Monadic (M: MONAD) =
struct
    let fold_free
        (f: int -> 'a -> 'a M.t)
        (term: t)
        (start: 'a)
        : 'a M.t
    =
        let rec fold term nb start =
            match term with

            | Value _ | Sort _ ->
                M.return start

            | Variable i when i < nb ->
                M.return start

            | Variable i ->
                f (i - nb) start

            | Typed (e, tp) ->
                M.(fold e nb start >>= fold tp nb)

            | Appl (f, a, _) ->
                M.(fold f nb start >>= fold a nb)

            | Lambda (tp, exp, _) ->
                M.(fold tp nb start >>= fold exp (nb + 1))

            | Pi (tp, rt, _) ->
                M.(fold tp nb start >>= fold rt (nb + 1))

            | Where (_, tp, exp, def) ->
                let open M in
                fold tp nb start
                >>= fold exp (nb + 1)
                >>= fold def nb
        in
        fold term 0 start


    let map_free
        (f: int -> int M.t)
        (term: t):
        t M.t
    =
        let open M in
        let rec map nb term =
            match term with
            | Sort _ | Value _ ->
                return term

            | Variable i when i < nb ->
                return (Variable i)

            | Variable i ->
                f (i - nb) >>= fun j ->
                return (Variable (j + nb))

            | Typed (exp, tp) ->
                map nb exp >>= fun exp ->
                map nb tp  >>= fun tp ->
                return (Typed (exp, tp))

            | Appl (f, a, info) ->
                map nb f >>= fun f ->
                map nb a >>= fun a ->
                return (Appl (f, a, info))

            | Lambda (tp, exp, info) ->
                map nb tp >>= fun tp ->
                map (nb + 1) exp >>= fun exp ->
                return (Lambda (tp, exp, info))

            | Pi (tp, res, info) ->
                map nb tp >>= fun tp ->
                map (nb + 1) res >>= fun res ->
                return (Pi (tp, res, info))

            | Where (name, tp, exp, value) ->
                map nb tp >>= fun tp ->
                map (nb + 1) exp >>= fun exp ->
                map nb value >>= fun value ->
                return (Where (name, tp, exp, value))
        in
        map 0 term
end (* Monadic *)


let split_application
    (t: t)
    : t * (t * Application_info.t) list
=
    let rec split t args =
        match t with
        | Appl (f, a, info) ->
            split f ((a,info) :: args)
        | _ ->
            t, args
    in
    split t []




let map (f: int -> int) (t: t): t =
    let module Mon = Monadic (Monad.Identity) in
    Monad.Identity.eval
        (Mon.map_free
            (fun i -> Monad.Identity.return (f i))
            t)




let up_from (start: int) (delta: int) (t:t): t =
    assert (0 <= delta);
    map
        (fun i ->
            if start <= i then
                i + delta
            else
                i)
        t


let up (delta: int) (t: t): t =
    if delta = 0 then
        t
    else
        up_from 0 delta t


let up1 (t: t): t =
    up 1 t




let down_from (start: int) (delta: int) (t: t): t option =
    assert (0 <= delta);
    let module Mon = Monadic (Option) in
    Mon.map_free
        (fun i ->
            if i < start then
                Some i
            else if start + delta <= i then
                Some (i - delta)
            else
                None
        )
        t


let down (delta:int) (t:t): t option =
  down_from 0 delta t





let rec substitute0 (f:int -> t) (beta_reduce: bool) (t:t): t =
    let rec sub t nb =
        match t with
        | Sort _ | Value _ ->
            t

        | Variable i when i < nb ->
            t

        | Variable i ->
            up nb (f @@ i - nb)

        | Typed (e, tp) ->
            Typed (sub e nb, sub tp nb)

        | Appl (f, a, mode) ->
            begin match f with
            | Lambda _ ->
                (* Old lambdas are not modified. *)
                Appl (sub f nb, sub a nb, mode)
            | _ ->
                let fnew = sub f nb in
                begin match fnew with
                | Lambda (_, exp, _ ) when beta_reduce ->
                    (* Substitution introduced a new lambda term as the function
                    term. Therefore we beta reduce. *)
                    apply exp (sub a nb)
                | _ ->
                    (* No new lambda term introduced. *)
                    Appl (fnew, sub a nb, mode)
                end
            end

        | Lambda (tp, exp, info) ->
           Lambda (sub tp nb, sub exp (nb + 1), info)

        | Pi (tp, rt, info) ->
           Pi (sub tp nb, sub rt (nb + 1), info)

        | Where (name, tp, exp, def) ->
            Where (name, sub tp nb, sub exp (nb + 1), sub def nb)
    in
    sub t 0


and apply (f: t) (a: t): t =
    substitute0
        (fun i ->
            if i = 0 then
                a
            else
                Variable (i - 1))
        false
        f


let substitute_with_beta (f: int -> t) (t: t): t =
    substitute0 f true t


let substitute (f:int -> t) (t:t): t =
    substitute0 f false t






let rec apply_nargs (f:t) (nargs:int) (mode: Application_info.t): t =
    if nargs = 0 then
        f
    else
        apply_nargs
            (Appl (f, Variable (nargs - 1), mode))
            (nargs - 1)
            mode









let has (p: int -> bool) (term: t): bool =
    let module Mon = Monadic (Option) in
    None
    = Mon.fold_free
        (fun j () ->
            if p j then
                None
            else
                Some ())
        term
        ()



let forall (p: int -> bool) (term: t): bool =
    not (has (fun i -> not (p i)) term)



let has_variable (i: int) (term: t): bool =
    has (fun j -> i = j) term




let fold_free (f: int -> 'a -> 'a) (term: t) (start: 'a): 'a =
    let module Mon = Monadic (Monad.Identity) in
    Monad.Identity.eval
        (Mon.fold_free
            (fun i start -> Monad.Identity.return (f i start))
            term
            start)




let make_product
        (name: string)
        (typed: bool)
        (kind: bool)
        (arg_typ: typ)
        (res_typ: typ)
    : t
    =
    assert (name <> "");
    let arrow = not (has_variable 0 res_typ) in
    let info = Pi_info.make name typed arrow kind in
    Pi (arg_typ, res_typ, info)




(* ----------------------------------------------------------- *)
(* Index to level conversion                                   *)
(* ----------------------------------------------------------- *)


let rec to_index (n: int) (term: t): t =
    match term with
    | Value _ | Sort _ ->
        term

    | Variable i ->
        Variable (bruijn_convert i n)

    | Appl (f, arg, info) ->
        Appl (to_index n f,
              to_index n arg,
              info)

    | Typed (term, typ) ->
        Typed (to_index n term,
               to_index n typ)

    | Lambda (typ, exp, info) ->
        Lambda (to_index n typ,
                to_index (n + 1) exp,
                info)

    | Pi (typ, rtyp, info) ->
        Pi (to_index n typ,
            to_index (n + 1) rtyp,
            info)

    | Where (name, tp, exp, def) ->
        Where (
            name,
            to_index n tp,
            to_index (n + 1) exp,
            to_index n def
        )


let to_level = to_index



(* ----------------------------------------------------------- *)
(* Inductive                                                   *)
(* ----------------------------------------------------------- *)

  module Inductive =
  struct
    let make_simple_inductive
        (base_count: int)
        (parameters: formal_argument list)
        (typ: formal_argument)
        (constructors: formal_argument list)
        : inductive
        =
        {base_count;
         n_up = 0;
         parameters;
         types = [| typ, Array.of_list constructors|]}

    type term = t

    type t =
        | Type of int * inductive
        | Constructor of int * int * inductive


    let is_simple (ind: inductive): bool =
        Array.length ind.types = 1

    module Type =
    struct
        type t = int * inductive

        let simple (ind: inductive): t =
            assert (is_simple ind);
            0, ind
        let _ = simple
    end

    let typ (ind: inductive): t =
        Type (0, ind)
    let _ = typ


    let constructor (i: int) (ind: inductive): t =
        assert (is_simple ind);
        Constructor (0, i, ind)
    let _ = constructor
  end
