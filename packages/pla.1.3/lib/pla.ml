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

module PlaBuffer = struct
   type dest =
      | File   of out_channel
      | Buffer of Buffer.t


   let appendToBuff (d:dest) (s:string) : unit =
      match d with
      | File(c)   -> output_string c s
      | Buffer(b) -> Buffer.add_string b s


   (** Text buffer use by Pla *)
   type t =
      {
         buffer           : dest;
         mutable indent   : int;
         mutable space    : string;
         mutable indented : bool;
      }

   let newBuffer () =
      {
         buffer   = Buffer(Buffer.create 128);
         indent   = 0;
         space    = "";
         indented = false;
      }

   let newFile (file:string) =
      {
         buffer   = File(open_out file);
         indent   = 0;
         space    = "";
         indented = false;
      }

   let contents (t:t) : string =
      match t.buffer with
      | Buffer(b) -> Buffer.contents b
      | File _ -> ""

   let close (t:t) : unit =
      match t.buffer with
      | Buffer _ -> ()
      | File(c) -> close_out c

   let newline (t:t) =
      appendToBuff t.buffer "\n";
      t.indented <- false

   let indent (t:t) : unit =
      t.indent <- t.indent +1;
      t.space  <- String.make (t.indent * 3) ' ';
      newline t

   let outdent (t:t) : unit =
      t.indent <- t.indent - 1;
      if t.indent < 0 then
         failwith "Cannot outdent more";
      t.space <- String.make (t.indent * 3) ' '

   let append (t:t) (s:string) : unit =
      if not t.indented then begin
         appendToBuff t.buffer t.space;
         t.indented <- true;
      end;
      appendToBuff t.buffer s

end

type buffer = PlaBuffer.t

type t = buffer -> unit

(* Builtin templates *)

let unit : t = fun _ -> ()

let newline : t = fun buffer -> PlaBuffer.newline buffer

let comma : t = fun buffer -> PlaBuffer.append buffer ","

let commaspace : t = fun buffer -> PlaBuffer.append buffer ", "

let semi : t = fun buffer -> PlaBuffer.append buffer ";"

let space : t = fun buffer -> PlaBuffer.append buffer " "

(* Templates of basic types *)

let string (s:string) : t =
   fun buffer -> PlaBuffer.append buffer s

let string_quoted (s:string) : t =
   fun buffer ->
   PlaBuffer.append buffer "\"";
   PlaBuffer.append buffer s;
   PlaBuffer.append buffer "\""

let int (i:int) : t =
   fun buffer -> PlaBuffer.append buffer (string_of_int i)

let float (f:float) : t =
   fun buffer -> PlaBuffer.append buffer (string_of_float f)

(* Functions to wrap templates *)

let quote (t:t) : t =
   fun buffer ->
   PlaBuffer.append buffer "\"";
   t buffer;
   PlaBuffer.append buffer "\""

let parenthesize (t:t) : t =
   fun buffer ->
   PlaBuffer.append buffer "(";
   t buffer;
   PlaBuffer.append buffer ")"

let indent (t:t) : t =
   fun buffer ->
   PlaBuffer.indent buffer;
   t buffer;
   PlaBuffer.outdent buffer

let wrap (l:t) (r:t) (t:t) : t =
   fun buffer ->
   l buffer;
   t buffer;
   r buffer

(* Functions to append templates *)

let append (t1:t) (t2:t) : t =
   fun buffer ->
   t1 buffer;
   t2 buffer

let join (elems:t list) : t =
   fun buffer -> List.iter (fun a -> a buffer) elems

let join_sep (sep:t) (elems:'a list) : t =
   fun buffer ->
   let rec loop = function
      | []   -> ()
      | [h]  -> h buffer
      | h::t ->
         h buffer;
         sep buffer;
         loop t
   in loop elems

let join_sep_all (sep:t) (elems:'a list) : t =
   fun buffer -> List.iter (fun h -> h buffer; sep buffer) elems

let map_join (f:'a -> t) (elems:'a list) : t =
   fun buffer -> List.iter (fun a -> f a buffer) elems

let map_sep (sep:t) (f:'a -> t) (elems:'a list) : t =
   fun buffer ->
   let rec loop = function
      | []   -> ()
      | [h]  -> (f h) buffer
      | h::t ->
         (f h) buffer;
         sep buffer;
         loop t
   in loop elems

let map_sep_all (sep:t) (f:'a -> t) (elems:'a list) : t =
   fun buffer -> List.iter (fun h -> (f h) buffer; sep buffer) elems

let (++) (t1:t) (t2:t) : t =
   append t1 t2

let print (t:t) : string =
   let buffer = PlaBuffer.newBuffer () in
   t buffer;
   PlaBuffer.contents buffer

let write (file:string) (t:t) : unit =
   let buffer = PlaBuffer.newFile file in
   t buffer;
   PlaBuffer.close buffer

