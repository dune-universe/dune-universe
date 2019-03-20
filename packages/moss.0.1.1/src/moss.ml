(* File: moss.ml

   Copyright (C) 2017-

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
   WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
   WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
   AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
   CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
   OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
   NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
   CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.  *)

open Printf
(* open Lwt *)

let min (x: int) (y: int) = if x <= y then x else y

type lang =
  | C | CC | Java | Ml | Pascal | Ada | Lisp | Scheme | Haskell
  | Fortran | Ascii | Vhdl | Perl | Matlab | Python | Mips | Prolog
  | Spice | VB | Csharp | Modula2 | A8086 | Javascript | Plsql

let string_of_lang = function
  | C -> "c"
  | CC -> "cc"
  | Java -> "java"
  | Ml -> "ml"
  | Pascal -> "pascal"
  | Ada -> "ada"
  | Lisp -> "lisp"
  | Scheme -> "scheme"
  | Haskell -> "haskell"
  | Fortran -> "fortran"
  | Ascii -> "ascii"
  | Vhdl -> "vhdl"
  | Perl -> "perl"
  | Matlab -> "matlab"
  | Python -> "python"
  | Mips -> "mips"
  | Prolog -> "prolog"
  | Spice -> "spice"
  | VB -> "vb"
  | Csharp -> "csharp"
  | Modula2 -> "modula2"
  | A8086 -> "a8086"
  | Javascript -> "javascript"
  | Plsql -> "plsql"

let server = "moss.stanford.edu"
let server_ip = Unix.inet_addr_of_string "171.64.78.49"
let port = 7690
let server_addr =
  try
    let h = Unix.gethostbyname server in
    Unix.ADDR_INET(h.Unix.h_addr_list.(0), port)
  with _ ->
    Unix.ADDR_INET(server_ip, port) (* fallback *)

(* userid seem to be of the type "[0-9]+".  However, we do not have
   guarantees about them, they could start with 0 or exceed the
   capacity of native integers (Perl can store numbers as "decimal
   strings"). *)
let check_userid ~err id =
  for i = 0 to String.length id - 1 do
    if id.[i] < '0' || '9' < id.[i] then invalid_arg err
  done

let default_userid =
  ref(try let id = Sys.getenv "MOSS_USERID" in
          check_userid id ~err:"The shell variable MOSS_USERID should only \
                                be made of digits 0-9.";
          id
      with Not_found -> "0")

let set_userid id =
  check_userid id ~err:"Moss.set_userid: the userid must only be \
                        made of digits 0-9";
  default_userid := id

let get_userid () = !default_userid


module File = struct
  class type in_obj_channel =
    object
      method input : Bytes.t -> int -> int -> int
      method close_in : unit -> unit
    end

  type data =
    | String of string (* content *)
    | Buffer of Buffer.t
    | File of string   (* full-path *)
    | Channel of (unit -> in_channel) (* (size, create-channel) *)
    | Obj of (unit -> in_obj_channel) (* (size, create-channel) *)

  type t = {
      name: string;
      mutable data: data;
      (* Some operations may alter the representation of the data. *)
      mutable size: int;  (* < 0 if it has to be computed *)
    }

  let name t = t.name

  let of_string ~name content =
    { name;  data = String content;  size = String.length content }

  let of_path ?name path =
    (* FIXME: check [name] and [path]? *)
    let name = match name with None -> path
                             | Some n -> n in
    (* The user can change directory but we to not want that the data
       becomes inaccessible because of that.  Keep a full path. *)
    let path = if Filename.is_relative path then
                 Filename.concat (Sys.getcwd()) path
               else path in
    { name;  data = File path;  size = Unix.((stat path).st_size) }

  let of_in_channel ?size ~name create =
    let size = match size with
      | Some sz ->
         if sz < 0 then
           invalid_arg "Moss.File.of_in_channel: ~size >= 0 required";
         sz
      | None -> -1 in
    { name;  data = Channel create;  size }

  let of_in_obj ?size ~name create_in_ch =
    let size = match size with
      | Some sz ->
         if sz < 0 then
           invalid_arg "Moss.File.of_in_obj: ~size >= 0 required";
         sz
      | None -> -1 in
    { name;  data = Obj create_in_ch;  size }

  (* Read until End_of_file into buffer [buf]. *)
  let read_in_buffer buf fh =
    set_binary_mode_in fh true;
    let b = Bytes.create 4096 in
    let len = ref 0 in
    while len := input fh b 0 4096;  !len > 0 do
      Buffer.add_subbytes buf b 0 !len;
    done

  let read_obj_in_buffer buf obj =
    let b = Bytes.create 4096 in
    try
      while true do
        let len = obj#input b 0 4096 in
        if len = 0 then
          (* Small pause to avoid a busy loop. *)
          ignore(Unix.select [] [] [] 0.1)
        else
          Buffer.add_subbytes buf b 0 len
      done
    with End_of_file -> ()

  (* May alter the data because, for some targets, it is necessary to
     read the data to determine the size. *)
  let size f =
    if f.size >= 0 then f.size
    else match f.data with
         | String _ | Buffer _ | File _ -> assert false
         | Channel create ->
            let fh = create () in
            let buf = Buffer.create 4096 in
            read_in_buffer buf fh;
            close_in fh;
            f.data <- Buffer buf;
            f.size <- Buffer.length buf;
            f.size
         | Obj create ->
            let obj = create() in
            let buf = Buffer.create 4096 in
            read_obj_in_buffer buf obj;
            obj#close_in();
            f.data <- Buffer buf;
            f.size <- Buffer.length buf;
            f.size

  exception Not_enough_data_on_channel
  (* Exception raised when a "file" does not contain enough data.  We
     have to abort the upload in this case because the file size was
     already sent. *)

  let copy_chunk_to_channel out_fh ~size in_fh =
    set_binary_mode_in in_fh true;
    let b = Bytes.create 4096 in
    let size = ref size in
    let len = ref 0 in
    while !size > 0 && (len := input in_fh b 0 (min 4096 !size);  !len > 0) do
      output out_fh b 0 !len;
      size := !size - !len;
    done;
    close_in in_fh;
    if !size > 0 then raise Not_enough_data_on_channel

  let copy_obj_to_channel out_fh ~size (obj: in_obj_channel) =
    let b = Bytes.create 4096 in
    try
      let size = ref size in
      while !size > 0 do
        let len = obj#input b 0 (min 4096 !size) in
        if len = 0 then
          (* Small pause to avoid a busy loop. *)
          ignore(Unix.select [] [] [] 0.1)
        else (
          output out_fh b 0 len;
          size := !size - len;
        )
      done;
      obj#close_in()
    with End_of_file ->
      obj#close_in();
      raise Not_enough_data_on_channel

  (* [id] = 0 ⇒ base file *)
  let upload out_fh file ~id ~lang =
    let size = size file in
    fprintf out_fh "file %d %s %d %s\n" id lang size file.name;
    try
      match file.data with
      | String c -> output_string out_fh c
      | Buffer buf -> Buffer.output_buffer out_fh buf
      | File path ->
         let fh = open_in path in
         copy_chunk_to_channel out_fh ~size fh
      | Channel create -> let fh = create () in
                          copy_chunk_to_channel out_fh ~size fh
      | Obj create -> let obj = create () in
                      copy_obj_to_channel out_fh ~size obj
    with Not_enough_data_on_channel ->
      failwith(sprintf "File %S has length less than the requested \
                        %d bytes" file.name size)
end


let submit ?(userid= !default_userid) ?(experimental=false) ?comment
      ?(by_dir=false) ?(max_rep=10) ?(n=250) lang ?(base=[]) files =
  check_userid userid ~err:"Moss.submit: userid must only be made of \
                            digits 0-9";
  let by_dir = if by_dir then '1' else '0' in
  let experimental = if experimental then '1' else '0' in
  if max_rep < 2 then invalid_arg "Moss.submit: ~max_rep must be >= 2";
  if n < 2 then invalid_arg "Moss.submit: ~n must be >= 2";
  let lang = string_of_lang lang in
  let comment = match comment with
    | Some c ->
       for i = 0 to String.length c - 1 do
         if c.[i] < ' ' || c.[i] > '}' then
           failwith "Moss.submit: comment can only contain printable \
                     ASCII characters"
       done;
       c
    | None -> "" in
  let in_fh, out_fh = Unix.open_connection server_addr in
  let close_sock () =
    output_string out_fh "end\n";
    flush out_fh;
    Unix.shutdown_connection in_fh in
  try
    (* FIXME: Do we want to introduce timeouts? *)
    fprintf out_fh "moss %s\ndirectory %c\nX %c\nmaxmatches %d\nshow %d\n\
                    language %s\n%!"
      userid by_dir experimental max_rep n lang;
    let r = input_line in_fh in
    if r = "no" then
      failwith ("Moos.connect: unrecognized language " ^ lang);
    (* Upload any base file. *)
    List.iter (fun fn -> File.upload out_fh fn ~id:0 ~lang) base;
    (* Upload other files. *)
    List.iteri (fun i fn -> File.upload out_fh fn ~id:(i+1) ~lang) files;
    (* Check not all sizes are = 0. *)
    if List.for_all (fun fn -> File.size fn = 0) files then
      failwith "Moss.submit: At least one file must have non-zero length";
    fprintf out_fh "query 0 %s\n%!" comment;
    let r, _, _ = Unix.select [Unix.descr_of_in_channel in_fh] [] [] 10. in
    match r with
    | [] -> failwith "Moss.submit: timeout to receive the URL"
    | _ :: _ ->
       match input_line in_fh with
       | url ->
          close_sock();
          Uri.of_string url
       | exception End_of_file ->
          failwith "Moss.submit: no URL returned by the server"
  with Failure _ as e ->
    close_sock();
    raise e

;;
(* Local Variables: *)
(* compile-command: "make -k -C.. build" *)
(* End: *)
