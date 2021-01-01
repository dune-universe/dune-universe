(*
 * Generic Transformers PPX syntax extension.
 * Copyright (C) 2016-2019
 *   Dmitrii Kosarev aka Kakadu
 * St.Petersburg State University, JetBrains Research
 *
 *)

open Ppxlib
open GTCommon

module E = Expander.Make(PpxHelpers)

let gt_param name =
  let open Deriving.Args in
  arg name __

let r =
  let open Deriving.Args in
  map1 ~f:(List.map (fun ({txt},e) -> (txt, e)) ) @@
  pexp_record __ none

let str_type_decl : (_, _) Deriving.Generator.t =
  Deriving.Generator.make Deriving.Args.(empty +> arg "options" r)
    (fun ~loc ~path info options ->
       let module H = Expander.Make(PpxHelpers) in
       (* Expander.notify "with annotations %s" (String.concat "," info); *)

       let generator_f si =
         H.str_type_decl_many_plugins ~loc si
           (match options with
            | None -> []
            | Some xs ->
              List.map (function
                  | (Lident name, e) -> begin
                      let extra =
                        match e.pexp_desc with
                        | Pexp_record (xs,_) -> List.map (fun ({txt},b) -> txt,b) xs
                        | Pexp_ident {txt = Lident s} when s = name -> []
                        | _ -> failwith "bad argument of a plugin"
                      in
                      (name,Expander.Use extra)
                  end
                  | _ -> failwith "only lowercase identifiers are allowed"
                ) xs
           )

       in
       generator_f [] info
    )

(*
let str_type_ext : (_, _) Deriving.Generator.t =
  Deriving.Generator.make Deriving.Args.(empty +> arg "options" r)
    (fun ~loc ~path info options ->
       let module H = Expander.Make(PpxHelpers) in
       (* Expander.notify "with annotations %s" (String.concat "," info); *)
       let generator_f si =
         H.str_type_ext_many_plugins ~loc si
           (match options with
            | None -> []
            | Some xs ->
              List.map (function
                  | (Lident name, e) -> begin
                      let extra =
                        match e.pexp_desc with
                        | Pexp_record (xs,_) -> List.map (fun ({txt},b) -> txt,b) xs
                        | _ -> failwith "asdf"
                      in
                      (name,Expander.Use extra)
                  end
                  | _ -> failwith "only lowercase identifiers are allowed"
                ) xs
           )

       in
       generator_f [] info
    )
*)

let sig_type_decl : (_, _) Deriving.Generator.t =
  Deriving.Generator.make Deriving.Args.(empty +> arg "options" r)
    (fun ~loc ~path info options ->
       let module H = Expander.Make(PpxHelpers) in
       (* Expander.notify "with annotations %s" (String.concat "," info); *)
       let generator_f si =
         H.sig_type_decl_many_plugins ~loc si
           (match options with
            | None -> []
            | Some xs ->
              List.map (function
                  | (Lident name, e) ->
                    (name,Expander.Use [])
                  | _ -> failwith "only lowercase identifiers are allowed"
                ) xs
           )

       in
       generator_f [] info
    )

let () =
  (*Sys.command "notify-send 'Registering deriver' gt" |> ignore;*)
  Deriving.add
    ~str_type_decl
    ~sig_type_decl
    (* ~str_type_ext *)
    (* ~sig_type_ext *)
    "gt"
  |> Deriving.ignore
