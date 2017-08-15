(* The MIT License (MIT)
   Copyright (c) 2016 Leonardo Laguna Ruiz

   Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
   associated documentation files (the "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be included in all copies or substantial
   portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
   LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
   WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

open Migrate_parsetree

open Ast_404
let ocaml_version = Versions.ocaml_404

let migrate = Versions.migrate Versions.ocaml_current ocaml_version

module Pla = struct
   open Ast_mapper
   open Asttypes
   open Parsetree
   open Ast_helper

   let readFile loc path =
      if Sys.file_exists path then
         let file = open_in path in
         let buffer = Buffer.create 16 in
         try
            while true do
               let c = input_char file in
               Buffer.add_char buffer c
            done;
            ""
         with | End_of_file ->
            close_in file;
            Buffer.contents buffer
      else
         let msg = Printf.sprintf "Cannot open the file '%s' from the current directory '%s'" path (Sys.getcwd ()) in
         prerr_endline msg;
         exit 1

   let buffer_id =  "__buffer__"

   let makeLident (str:string) : Longident.t Location.loc =
      Longident.parse str |> Location.mknoloc

   let buffer  = makeLident buffer_id
   let newline = makeLident "Pla.PlaBuffer.newline"
   let indent  = makeLident "Pla.PlaBuffer.indent"
   let outdent = makeLident "Pla.PlaBuffer.outdent"
   let append  = makeLident "Pla.PlaBuffer.append"
   let pint    = makeLident "Pla.int"
   let pfloat  = makeLident "Pla.float"
   let pstring = makeLident "Pla.string"
   let unit    = Exp.construct (makeLident "()") None

   let offsetPosition (displacement:int) (pos1:Lexing.position) (pos2:Lexing.position) : Lexing.position =
      Lexing.{
         pos1 with
         pos_lnum = pos1.pos_lnum + pos2.pos_lnum - 1;
         pos_bol  = pos1.pos_bol  + pos2.pos_bol;
         pos_cnum = pos1.pos_cnum + pos2.pos_cnum + displacement ;
      }

   let offsetLocation (displacement:int) (loc1:Location.t) (loc2:Location.t) : Location.t =
      Location.{
         loc1 with
         loc_start = offsetPosition displacement loc1.loc_start loc2.loc_start;
         loc_end   = offsetPosition displacement loc1.loc_start loc2.loc_end;
      }

   let mkVar (var_loc:Location.t) (v:string) =
      Location.mkloc (Longident.parse v) var_loc

   let template_type (t:Pla_tokens.vartype) =
      match t with
      | Pla_tokens.Int      -> Typ.constr (makeLident "int") []
      | Pla_tokens.Float    -> Typ.constr (makeLident "float") []
      | Pla_tokens.String   -> Typ.constr (makeLident "string") []
      | Pla_tokens.Template -> Typ.constr (makeLident "Pla.t") []

   let no_label = Nolabel
   let constString s = Const.string s

   let makeExp (loc:Location.t) (displacement:int) (s:Pla_tokens.s) : expression =
      match s with
      | Pla_tokens.N ->
         Exp.apply (Exp.ident newline) [no_label, Exp.ident buffer]
      | Pla_tokens.I ->
         Exp.apply (Exp.ident indent)  [no_label, Exp.ident buffer]
      | Pla_tokens.O ->
         Exp.apply (Exp.ident outdent) [no_label, Exp.ident buffer]
      | Pla_tokens.T txt ->
         Exp.apply (Exp.ident append) [no_label, Exp.ident buffer; no_label, Exp.constant(constString txt)]
      | Pla_tokens.V(v,vartype,loc_ref) ->
         let var_loc = offsetLocation displacement loc loc_ref in
         let v_exp = Exp.constraint_ ~loc:var_loc (Exp.ident ~loc:var_loc (mkVar var_loc v)) (template_type vartype) in
         match vartype with
         | Pla_tokens.Template -> Exp.apply v_exp [no_label, Exp.ident buffer]
         | Pla_tokens.Int      -> Exp.apply (Exp.ident pint) [no_label,v_exp; no_label, Exp.ident buffer]
         | Pla_tokens.Float    -> Exp.apply (Exp.ident pfloat) [no_label,v_exp; no_label, Exp.ident buffer]
         | Pla_tokens.String   -> Exp.apply (Exp.ident pstring) [no_label,v_exp; no_label, Exp.ident buffer]

   let makeExpSeq (loc:Location.t) (displacement:int) (sl:Pla_tokens.s list) : expression =
      List.fold_right (fun a s -> Exp.sequence (makeExp loc displacement a) s) sl unit

   let makeTemplateExp (loc:Location.t) (displacement:int) (sl:Pla_tokens.s list) : expression =
      let pat = Pat.var (Location.mknoloc buffer_id) in
      let fun_ = Exp.fun_ ~loc no_label None pat (makeExpSeq loc displacement sl) in
      Exp.constraint_ ~loc fun_ (template_type Pla_tokens.Template)

   let mapper _config _cookies =
      { default_mapper with
        expr = fun mapper expr ->
           match expr with
           | { pexp_desc = Pexp_constant (Pconst_string (text, Some "pla")); pexp_loc = loc } ->
              let tokens = PlaLex.tokenize text in
              let displacement = 5 in
              let pla_exp = makeTemplateExp loc displacement tokens in
              pla_exp

           | { pexp_desc =
                  Pexp_extension
                     ({txt = "pla"}, PStr [{pstr_desc = Pstr_eval({pexp_desc = Pexp_constant(Pconst_string (text, _)) ; pexp_loc = loc }, _) }]) } ->
              let tokens = PlaLex.tokenize text in
              let displacement = 2 in
              let pla_exp = makeTemplateExp loc displacement tokens in
              pla_exp

           (* Files as templates *)

           | { pexp_desc = Pexp_constant (Pconst_string (path, Some "pla_file")); pexp_loc = loc } ->
              let text = readFile loc path in
              let tokens = PlaLex.tokenize text in
              let displacement = 5 in
              let pla_exp = makeTemplateExp loc displacement tokens in
              pla_exp

           | { pexp_desc =
                  Pexp_extension
                     ({txt = "pla_file"}, PStr [{pstr_desc = Pstr_eval({pexp_desc = Pexp_constant(Pconst_string (path, _)) ; pexp_loc = loc }, _) }]) } ->
              let text = readFile loc path in
              let tokens = PlaLex.tokenize text in
              let displacement = 2 in
              let pla_exp = makeTemplateExp loc displacement tokens in
              pla_exp

           | _ -> default_mapper.expr mapper expr;
      }

end

let args_spec = []

let reset_args () = ()

let () =
   Driver.register
      ~name:"pla" ~args:args_spec ~reset_args
      ocaml_version Pla.mapper
