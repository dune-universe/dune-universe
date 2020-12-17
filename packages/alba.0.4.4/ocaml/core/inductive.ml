(* A pure representation of inductive types. *)

open Fmlib
open Common


type params = (string * Term.typ) array (* Valid in the initial context. *)


let push_params (n: int) (params: params) (res: Term.typ): Term.typ =
    (* Push parameters in front of a type. For an inductive kind [n] must be
    zero. For a constructor type [n] must be the number of types. *)
    Array.foldi_right
        (fun iparam (name, typ) res ->
            Term.(Pi (up_from iparam n typ, res, Pi_info.typed name)))
        params
        res



module Header =
struct
    type t = {
        name: string;
        kind:  Term.typ; (* only indices, valid in a context with parameters *)
        indices: (Term.Pi_info.t * Term.typ) array; (* kind arguments *)
        sort:  Sort.t;
    }
    (* [indices] and [sort] represent the normalized version of [kind]*)


    let make name kind indices sort =
        {name; kind; indices = Array.of_list indices; sort}


    let name header = header.name


    let count_indices (header: t): int =
        Array.length header.indices

    let has_index (header: t): bool =
        0 <> count_indices header


    let kind (params: params) (header: t): Term.typ =
        push_params 0 params header.kind


    let default_type
        (i:int) (params: params) (headers: t array)
        : Term.typ
    =
        (* Valid in a context with the types and the parameters. *)
        let nparams = Array.length params
        and ntypes  = Array.length headers
        in
        let rec make k typ =
            if k = nparams then
                typ
            else
                make
                    (k + 1)
                    Term.(
                        application
                            typ
                            (Variable (bruijn_convert k nparams))
                    )
        in
        make 0 Term.(Variable (bruijn_convert i ntypes + nparams))



    let is_well_constructed
        (i: int)
        (params: params)
        (headers: t array)
        (nargs: int)
        (typ: Term.typ)
        : bool
    =
        (* Check that [typ] has the form [I p1 p2 ... i1 i2 ...] where [I]
        corresponds to the [i]th inductive type, [p1 p2 ...] are the parameters
        and [i1 i2 ... ] are the indices. *)
        let open Term in
        let nparams = Array.length params
        and ntypes  = Array.length headers
        and f, params_index = split_application typ in
        let params_index = Array.of_list params_index
        in
        let inductive_variable =
            Variable (bruijn_convert i (ntypes + nparams + nargs))
        and param_variable k =
            Variable (bruijn_convert k (nparams + nargs))
        in
        f = inductive_variable
        &&
        Common.Interval.forall
            (fun k ->
                assert (k < Array.length params_index);
                param_variable k = fst params_index.(k)
            )
            0 nparams
end (* Header *)





module Constructor =
struct
    type t = {
        name: string;
        typ:  Term.typ; (* Valid in context with all types of the family and the
        parameters. *)
    }

    let make name typ =
        {name; typ}

    let get co =
        co.name, co.typ
end




module Type =
struct
    type t = {
        nprevious: int; (* number of previous constructors *)
        header: Header.t;
        constructors: Constructor.t array;
    }

    let make nprevious header constructors =
        {nprevious; header; constructors}
end (* Type *)




type t = {
    n_up: int;

    params: params;

    positive_params: Int_set.t;

    types: Type.t array;
}


let make params positive_params types =
    {n_up = 0; params; positive_params; types}


let up (n: int) (ind: t): t =
    {ind with n_up = n + ind.n_up}



let count_types (ind: t): int =
    Array.length ind.types


let count_params (ind: t): int =
    Array.length ind.params


let is_param_positive (iparam: int) (ind: t): bool =
    iparam < count_params ind
    &&
    Common.Int_set.mem iparam ind.positive_params


let parameter_name (iparam: int) (ind: t): string =
    assert (iparam < count_params ind);
    fst ind.params.(iparam)


let parameters (ind: t): params =
    let n = ind.n_up + count_types ind
    in
    Array.map (fun (name, typ) -> name, Term.up n typ) ind.params


let ith_type (i: int) (ind: t): string * Term.typ =
    assert (i < count_types ind);
    let header = ind.types.(i).header
    in
    Header.name header
    ,
    Term.up
        ind.n_up
        (Header.kind ind.params header)


let count_constructors (i: int) (ind: t): int =
    assert (i < count_types ind);
    Array.length ind.types.(i).constructors



let count_previous_constructors (i: int) (ind: t): int =
    assert (i < count_types ind);
    ind.types.(i).nprevious



let raw_constructor (i: int) (j: int) (ind: t): string * Term.typ =
    assert (i < count_types ind);
    assert (j < count_constructors i ind);
    let name, typ =
        Constructor.get ind.types.(i).constructors.(j)
    in
    name,
    Term.up_from (count_params ind + count_types ind) ind.n_up typ



let constructor (i: int) (j: int) (ind: t): string * Term.typ =
    let ntypes = count_types ind
    in
    assert (i < ntypes);
    assert (j < count_constructors i ind);
    let name, typ =
        Constructor.get ind.types.(i).constructors.(j)
    in
    let typ = push_params ntypes ind.params typ in
    name,
    Term.up_from
        ntypes
        ind.n_up
        typ
