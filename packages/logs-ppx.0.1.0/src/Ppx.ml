open Ppxlib
module Builder = Ast_builder.Default

let expander ~ctxt e l =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let log_loc, log_type =
    match e.pexp_desc with
    | Pexp_ident { loc; txt = Lident log_type } -> (loc, log_type)
    | _ -> (loc, "info")
  in
  Builder.eapply ~loc
    (Builder.pexp_ident ~loc:log_loc
       { txt = Ldot (Lident "Logs", log_type); loc })
    [
      Builder.pexp_fun ~loc Nolabel None
        (Builder.ppat_var ~loc { txt = "m"; loc })
        (Builder.eapply ~loc
           (Builder.pexp_ident ~loc { txt = Lident "m"; loc })
           (List.map snd l));
    ]

let extension =
  let pattern =
    let open Ast_pattern in
    (* this grabs the first argument from the apply and
       then passes it into Log.sexp's [log] parameter.
       All the arguments of apply are parsed as a message. *)
    pstr (pstr_eval (pexp_apply __ __) nil ^:: nil)
  in
  Context_free.Rule.extension
    (Extension.V3.declare "log" Expression pattern expander)

let () = Driver.register_transformation ~rules:[ extension ] "logs-ppx"
