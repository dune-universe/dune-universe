
open Ez_subst.V1

let () =
  Printf.printf "%s\n%!"
    ( EZ_SUBST.string
        ~paren:(fun ctxt s ->
           match s with
            | "y" -> "Y"
            | "xY" -> "Z"
            | _ -> ctxt)
        ~ctxt: "DEFAULT"
        "$(x$(y))"
    )
