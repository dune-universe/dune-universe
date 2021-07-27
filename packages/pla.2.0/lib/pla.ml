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

type buffer =
  { append : string -> unit
  ; content : unit -> string
  ; close : unit -> unit
  ; mutable indent : int
  ; mutable space : string
  ; mutable indented : bool
  }

type t = buffer -> unit

let buffer_new () =
  let b = Buffer.create 128 in
  { append = (fun s -> Buffer.add_string b s)
  ; content = (fun () -> Buffer.contents b)
  ; close = (fun () -> Buffer.clear b)
  ; indent = 0
  ; space = ""
  ; indented = false
  }


let buffer_new_file (file : string) =
  let c = open_out file in
  { append = (fun s -> output_string c s)
  ; content = (fun () -> "")
  ; close = (fun () -> close_out c)
  ; indent = 0
  ; space = ""
  ; indented = false
  }


let buffer_contents t : string = t.content ()

let buffer_close t : unit = t.close ()

let buffer_newline t =
  t.append "\n" ;
  t.indented <- false
  [@@inline always]


let buffer_indent t : unit =
  t.indent <- t.indent + 1 ;
  t.space <- String.make (t.indent * 3) ' ' ;
  buffer_newline t
  [@@inline always]


let buffer_outdent t : unit =
  t.indent <- t.indent - 1 ;
  if t.indent < 0 then
    failwith "Cannot outdent more" ;
  t.space <- String.make (t.indent * 3) ' '
  [@@inline always]


let buffer_append t (s : string) : unit =
  if not t.indented then begin
    t.append t.space ;
    t.indented <- true
  end ;
  t.append s
  [@@inline always]


let buffer_apply (t : t) (buffer : buffer) : unit = t buffer [@@inline always]

let make (f : buffer -> unit) : t = f [@@inline always]

(* Builtin templates *)

let unit : t = fun _ -> ()

let newline : t = fun buffer -> buffer_newline buffer

let comma : t = fun buffer -> buffer_append buffer ","

let commaspace : t = fun buffer -> buffer_append buffer ", "

let semi : t = fun buffer -> buffer_append buffer ";"

let space : t = fun buffer -> buffer_append buffer " "

(* Templates of basic types *)

let string (s : string) : t = fun buffer -> buffer_append buffer s

let string_quoted (s : string) : t =
 fun buffer ->
  buffer_append buffer "\"" ;
  buffer_append buffer s ;
  buffer_append buffer "\""


let int (i : int) : t = fun buffer -> buffer_append buffer (string_of_int i)

let float (f : float) : t =
 fun buffer ->
  let s = string_of_float f in
  buffer_append buffer s ;
  if s.[String.length s - 1] = '.' then
    buffer_append buffer "0"


let bool (b : bool) : t =
 fun buffer ->
  let s = if b then "true" else "false" in
  buffer_append buffer s


(* Functions to wrap templates *)

let quote (t : t) : t =
 fun buffer ->
  buffer_append buffer "\"" ;
  t buffer ;
  buffer_append buffer "\""


let parenthesize (t : t) : t =
 fun buffer ->
  buffer_append buffer "(" ;
  t buffer ;
  buffer_append buffer ")"


let indent (t : t) : t =
 fun buffer ->
  buffer_indent buffer ;
  t buffer ;
  buffer_outdent buffer


let wrap (l : t) (r : t) (t : t) : t =
 fun buffer ->
  l buffer ;
  t buffer ;
  r buffer


(* Functions to append templates *)

let append (t1 : t) (t2 : t) : t =
 fun buffer ->
  t1 buffer ;
  t2 buffer


let join (elems : t list) : t = fun buffer -> List.iter (fun a -> a buffer) elems

let join_sep (sep : t) (elems : 'a list) : t =
 fun buffer ->
  let rec loop = function
    | [] -> ()
    | [ h ] -> h buffer
    | h :: t ->
        h buffer ;
        sep buffer ;
        loop t
  in
  loop elems


let join_sep_all (sep : t) (elems : 'a list) : t =
 fun buffer ->
  List.iter
    (fun h ->
      h buffer ;
      sep buffer )
    elems


let map_join (f : 'a -> t) (elems : 'a list) : t = fun buffer -> List.iter (fun a -> f a buffer) elems

let map_sep (sep : t) (f : 'a -> t) (elems : 'a list) : t =
 fun buffer ->
  let rec loop = function
    | [] -> ()
    | [ h ] -> (f h) buffer
    | h :: t ->
        (f h) buffer ;
        sep buffer ;
        loop t
  in
  loop elems


let map_sep_all (sep : t) (f : 'a -> t) (elems : 'a list) : t =
 fun buffer ->
  List.iter
    (fun h ->
      (f h) buffer ;
      sep buffer )
    elems


let ( ++ ) (t1 : t) (t2 : t) : t = append t1 t2

let print (t : t) : string =
  let buffer = buffer_new () in
  t buffer ;
  buffer_contents buffer


let write (file : string) (t : t) : unit =
  let buffer = buffer_new_file file in
  t buffer ;
  buffer_close buffer


type compiled =
  { tokens : Pla_tokens.s list
  ; slots : (string * Pla_tokens.vartype) list
  ; filled : (string * t) list
  }

module StringMap = Map.Make (String)

let collectSlots tokens =
  let rec loop acc t =
    match t with
    | Pla_tokens.V (name, type_, _) :: t -> loop (StringMap.add name type_ acc) t
    | _ :: t -> loop acc t
    | [] -> acc
  in
  loop StringMap.empty tokens


let create text =
  let tokens = Pla_lex.tokenize text in
  let slots = StringMap.fold (fun key value acc -> (key, value) :: acc) (collectSlots tokens) [] in
  { tokens; slots; filled = [] }


let set_generic name value type_ compiled =
  let rec loop acc slots =
    match slots with
    | (var, found_type_) :: t when var = name ->
        if type_ = found_type_ then
          (var, value), acc @ t
        else
          failwith ("The type for template does not match: " ^ name)
    | h :: t -> loop (h :: acc) t
    | [] ->
        let available = String.concat ", " (List.map (fun (s, _) -> s) compiled.slots) in
        failwith ("The slot '" ^ name ^ "' could not be filled. " ^ available)
  in
  let filled1, slots = loop [] compiled.slots in
  { compiled with filled = filled1 :: compiled.filled; slots }


let set name value compiled = set_generic name value Pla_tokens.Template compiled

let seti name value compiled = set_generic name (int value) Pla_tokens.Int compiled

let setf name value compiled = set_generic name (float value) Pla_tokens.Float compiled

let sets name value compiled = set_generic name (string value) Pla_tokens.String compiled

let close compiled buffer =
  if compiled.slots <> [] then
    let available = String.concat ", " (List.map (fun (s, _) -> s) compiled.slots) in
    failwith ("The template is not filled. " ^ available)
  else
    let open Pla_tokens in
    let rec loop tokens =
      match tokens with
      | N :: t ->
          buffer_newline buffer ;
          loop t
      | I :: t ->
          buffer_indent buffer ;
          loop t
      | O :: t ->
          buffer_outdent buffer ;
          loop t
      | T s :: t ->
          buffer_append buffer s ;
          loop t
      | V (name, _, _) :: t ->
          let temp = List.assoc name compiled.filled in
          temp buffer ;
          loop t
      | [] -> ()
    in
    loop compiled.tokens
