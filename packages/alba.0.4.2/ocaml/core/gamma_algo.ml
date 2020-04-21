open Fmlib

module type GAMMA =
sig
    type t

    val count: t -> int
    val push_local: string -> Term.typ -> t -> t
    val type_of_literal:    Term.Value.t -> t -> Term.typ
    val type_of_variable: int -> t -> Term.typ
    val definition_term: int -> t -> Term.t option
end



module Make (Gamma: GAMMA) =
struct
    include Gamma

    let key_split
          (t: Term.t)
          (args: (Term.t * Term.Application_info.t) list)
          (c: t)
        : Term.t * (Term.t * Term.Application_info.t) list
      =
      let rec split t args =
        match t with
        | Term.Variable i ->
           (match definition_term i c with
            | None ->
               t, args
            | Some def ->
               split def args)

        | Lambda (_, exp, _) ->
            (
                match args with
                | [] ->
                    t, args
                | (arg, _) :: args ->
                    split Term.(apply exp arg) args
            )

        | Term.Appl (f, arg, mode) ->
           split f ((arg, mode) :: args)

        | Term.Typed (term, _) ->
            term, args

        | _ ->
           t, args
      in
      split t args



    let key_normal (t: Term.t) (c: t): Term.t =
        let key, args = key_split t [] c in
        List.fold_left
            (fun res (arg, mode) ->
              Term.Appl (res, arg, mode))
            key
            args



    let type_of_term (t:Term.t) (c:t): Term.typ =
        let rec typ t c =
            let open Term in
            match t with
            | Sort s ->
                type_of_sort s

            | Value v ->
                type_of_literal v c

            | Variable i ->
                type_of_variable i c

            | Typed (_, tp) ->
               tp

            | Appl (f, a, _) ->
               (match key_normal (typ f c) c with
                | Pi (_, rt, _) ->
                   apply rt a
                | _ ->
                   assert false (* Illegal call! Term is not welltyped. *)
               )

            | Lambda (tp, exp, info) ->
                let c_inner = push_local (Lambda_info.name info) tp c in
                let rt      = typ exp c_inner
                in
                let info =
                    if has_variable 0 rt then
                        Pi_info.typed (Lambda_info.name info)
                    else
                        Pi_info.arrow
                in
                Pi (tp, rt, info)

            | Pi (tp, rt, info) ->
               let name = Pi_info.name info in
               (match
                  typ tp c, typ rt (push_local name tp c)
                with
                | Sort s1, Sort s2 ->
                  let open Sort in
                  (match s1, s2 with
                    | Proposition, Any i ->
                      Sort (Any i)

                    | Any i, Any j ->
                      Sort (Any (max i j))

                    | _, Proposition ->
                      Sort Proposition
                  )

                | _, _ ->
                   assert false (* Illegal call: term is not welltyped! *)
               )

            | Where (name, tp, exp, def) ->
                typ (expand_where name tp exp def) c
        in
        typ t c



    let rec sort_of_kind (k: Term.typ) (c:t): Term.Sort.t option =
        let open Term
        in
        match key_normal k c with
        | Sort s ->
            Some s
        | Pi (arg_tp, res_tp, _) ->
            sort_of_kind res_tp (push_local "_" arg_tp c)
        | _ ->
            None


    let is_kind (k: Term.typ) (c: t): bool =
        Option.has (sort_of_kind k c)

end
