(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019,2020 DaiLambda, Inc. <contact@dailambda.jp>            *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)
open Utils.Open

module Int64 = Stdint.Int64
  
(* XXX Problem in 32bit arch
     
   * [Cstruct.of_bigarray] only takes [char] indexed [Bigstring.t].
   * Offset must be [int] in [Cstruct].
   
   The current simple implementation to map the entire file to one [Bigstring.t]
   restricts the maximum file size in 32bit arch to [1_073_741_823], which is roughly just 1GB.
*)

module C = Xcstruct

type mode =
  | Private
  | Reader
  | Writer

let is_shared = function
  | Private -> false
  | Reader | Writer -> true

type storage = {
  mutable array : Bigstring.t ;
  (* mmaped array where the nodes are written and indexed. *)

  mutable current_length : Index.t ;
  (* Current length of the node table.  
     The next index number to be used. *)

  mutable mapped_length : Index.t ;

  mutable last_root_index : Index.t option ;
  
  fd : Unix.file_descr ; 
  (* File descriptor to the mapped file *)

  pos : int64 ; 
  (* Position of the first cell in the file *)

  mode : mode ;

  version : int ;
  fn : string ;

  resize_step : Index.t
  (* How much space allocated for each resize, in cells *)
}

type t = storage

let set_last_root_index t x  = t.last_root_index <- x
let get_last_root_index t    = t.last_root_index

let get_current_length t = t.current_length

let size t = Stdint.Int64.(Index.to_int64 (get_current_length t ) * 32L)

let filename t = t.fn

let mode t = t.mode

(* Constants *)

(* 2^32 - 256 .. 2^32 - 1 are used for tags for nodes
   
   Max index: 4G = 4_294_967_040
   The maximum data size is about 128GB
*)
let max_index = Index.(max_int - of_int 256)

let bytes_per_cell = 32L


(* Resize *)

let make_array fd ~pos mode mapped_length =
  let shared = is_shared mode in
  let open Bigarray in
  let size = Int64.(of_uint32 mapped_length * bytes_per_cell) in
  let size = 
    if size > Int64.of_int Stdlib.max_int then 
      failwithf "Size %Ld is too big in this archtecture" (Int64.of_uint32 mapped_length)
    else Int64.to_int size
  in
  array1_of_genarray @@ Unix.map_file fd ~pos char c_layout shared [| size |] 

let resize required t =
  let open Index in
  assert (t.mode = Writer);
  let new_mapped_length = 
    ((required - t.mapped_length) / t.resize_step + Index.one) * t.resize_step  + t.mapped_length
  in
  Log.notice "Storage: resizing to %Ld" (Index.to_int64 new_mapped_length);
  let array = make_array t.fd ~pos:t.pos t.mode (Index.to_uint32 new_mapped_length) in
  t.array <- array;
  t.mapped_length <- new_mapped_length

let may_resize =
  fun required t ->
    if t.mapped_length < required then 
      resize required t
    else ()


(* Access *)

let get_cell t i =
  (* XXX May overflow in 32bits arch! *)
  let i = Index.to_int i in 
  (* may raise Invalid_argument if the offset is too far away *)
  C.of_bigarray ~off:(i*32) ~len:32 t.array

let get_bytes t i n =
  (* XXX May overflow in 32bits arch! *)
  let i = Index.to_int i in 
  C.of_bigarray ~off:(i*32) ~len:n t.array

let new_index c =
  (* XXX check of size *)
  let i = c.current_length in
  let i' = Index.succ i in
  c.current_length <- i';
  may_resize i' c;
  i

let new_indices c n =
  (* XXX check of size *)
  assert (n > 0);
  let i = c.current_length in
  let i' = Index.(i + of_int n) in
  c.current_length <- i';
  may_resize i' c;
  i

module Header = struct
  type t = 
    { last_next_index : Index.t
    ; last_root_index : Index.t option
    }

  (*
  Cell #1
  |0                   19|20         23|24        27|28        31|
  |< hash of the right               ->|<- i root ->|<- i next ->|
  
  Cell #2
  |0                   19|20         23|24        27|28        31|
  |< hash of the right               ->|<- i root ->|<- i next ->|
  *)
    
  module Blake2B_24 = struct
    open Blake2.Blake2b
  
    let of_string s =
      let b = init 24 in
      update b (Bytes.of_string s);
      let Hash bs = final b in
      Bytes.to_string bs
  end
  
  let raw_read' array i =
    let cstr = C.of_bigarray ~off:(i*32) ~len:32 array in
    let last_next_index = C.get_index cstr 28 in
    let last_root_index = Index.zero_then_none @@ C.get_index cstr 24 in
    let h = C.copy cstr 0 24 in
    let string_24_31 = C.copy cstr 24 8 in
    let h' = Blake2B_24.of_string string_24_31 in
    if h <> h' then None
    else Some { last_next_index ; last_root_index }

  let raw_read array =
    match raw_read' array 1 with
    | Some x -> Some x
    | None -> (* something wrong in the cell #1 *)
        match raw_read' array 2 with
        | Some x -> Some x
        | None -> None (* something wrong in the cell #2 *)
      
  let _read t = raw_read t.array

  let write' t i { last_next_index ; last_root_index } =
    if t.mode = Reader then invalid_arg "Reader cannot write";
    let cstr = get_cell t @@ Index.of_int i in
    C.set_index cstr 28 last_next_index;
    C.set_index cstr 24 (Option.default Index.zero last_root_index);
    let string_24_31 = C.copy cstr 24 8 in
    let h = Blake2B_24.of_string string_24_31 in
    C.write_string h cstr 0 24

  let write t x =
    (* The write is NOT atomic but corruption can be detected by the checksum 
       and the double writes
    *)
    write' t 1 x;
    write' t 2 x
  
  let commit t =
    let cp = { last_next_index = t.current_length
             ; last_root_index = t.last_root_index } 
    in
    write t cp
      
end

let commit = Header.commit

let default_resize_step = Index.of_int 1000_000 (* 32MB *)

let create 
    ?(pos=0L) 
    ?length 
    ?(resize_step=default_resize_step)
    fn =
  let fd = Unix.openfile fn [O_CREAT; O_EXCL; O_RDWR] 0o644 in
  let mapped_length = 
    match length with 
    | None -> resize_step 
    | Some i ->
        match Sys.int_size with
        | 31 | 32 -> Index.of_int i
        | 63 ->
            if i > Index.(to_int max_int) then failwithf "create: too large: %d@." i
            else Index.of_int i
        | _ -> assert false
  in
  let array = make_array fd ~pos Writer (Index.to_uint32 mapped_length) in

  let version = 1 in (* XXX make it global? *)

  let cstr = C.of_bigarray ~off:0 ~len:32 array in
  C.blit_from_string ("PLEBEIA " ^ String.make 28 '\000') 0 cstr 0 32;
  C.set_index cstr 24 (Index.of_int version); (* XXX size check *)
  let t = 
    { array ;
      mapped_length ;
      current_length = Index.of_int 3; (* #0 for PLEBEIA..., #1 and #2 for header *)
      last_root_index = None ;
      fd ; 
      pos ;
      mode= Writer;
      version ;
      fn ;
      resize_step
    }
  in
  Header.commit t;
  t

let truncate ?length t =
  Unix.ftruncate t.fd (Int64.to_int t.pos);
  let mapped_length = 
    match length with 
    | None -> t.resize_step 
    | Some i ->
        match Sys.int_size with
        | 31 | 32 -> Index.of_int i
        | 63 ->
            if i > Index.(to_int max_int) then failwithf "create: too large: %d@." i
            else Index.of_int i
        | _ -> assert false
  in
  let array = make_array t.fd ~pos:t.pos t.mode (Index.to_uint32 mapped_length) in

  let version = 1 in (* XXX make it global? *)

  let cstr = C.of_bigarray ~off:0 ~len:32 array in
  C.blit_from_string ("PLEBEIA " ^ String.make 28 '\000') 0 cstr 0 32;
  C.set_index cstr 24 (Index.of_int version); (* XXX size check *)

  t.array <- array;
  t.mapped_length <- mapped_length;
  t.current_length <- Index.of_int 3; (* #0 for PLEBEIA..., #1 and #2 for header *)
  t.last_root_index <- None ;
  Header.commit t

let open_ ?(pos=0L) ?(resize_step=default_resize_step) ~mode fn =
  if not @@ Sys.file_exists fn then 
    if mode <> Private then create ~pos fn 
    else failwithf "%s: file not found" fn
  else begin
    let fd = Unix.openfile fn [O_RDWR] 0o644 in
    let st = Unix.LargeFile.fstat fd in
    let sz = Int64.sub st.Unix.LargeFile.st_size pos in
    assert (Int64.rem sz 32L = 0L);  (* XXX think about the garbage *)
    let cells = Int64.(sz / 32L) in 
    if cells > Index.to_int64 max_index then assert false;
    let mapped_length = Index.of_int64 cells in
    let array = make_array fd ~pos mode (Index.to_uint32 mapped_length) in

    let cstr = C.of_bigarray ~off:0 ~len:32 array in
    if not (C.copy cstr 0 8 = "PLEBEIA ") then failwith "This is not Plebeia data file";
    let version = Index.to_int @@ C.get_index cstr 24 in

    if version <> 1 then (* XXX make it global *) begin
      Log.fatal "ERROR: version mismatch.  The file's version is %d but the system expects %d.  We keep going forward." version 1;
      exit 2;
    end;

    match Header.raw_read array with
    | None -> failwithf "Failed to load header"
    | Some h ->
        { array ;
          mapped_length ;
          current_length = h.Header.last_next_index;
          last_root_index = h.Header.last_root_index;
          fd = fd ;
          pos ; 
          mode ;
          version ;
          fn ; 
          resize_step
        }
  end

let close ({ fd ; mode ; _ } as t) =
  if mode <> Reader then Header.commit t;
  Unix.close fd

let reopen storage =
  (* We load the header first, before fstat the file.

     If we would do opposite, the following might happen:

     * Reader fstats
     * Writer extends the file, making the fstats obsolete
     * Write update the header, last_indices point out of the obsolete fstat
     * Reader reads the header
     * Reader fails to load the last_indices, since it is not mapped
  *)
  match Header.raw_read storage.array with
  | None -> failwithf "Failed to load header"
  | Some h ->
      let st = Unix.LargeFile.fstat storage.fd in
      let sz = Int64.sub st.Unix.LargeFile.st_size storage.pos in
      assert (Int64.rem sz 32L = 0L);  (* XXX think about the garbage *)
      let cells = Int64.(sz / 32L) in 
      if cells > Index.to_int64 max_index then assert false;
      let mapped_length = Index.of_int64 cells in
      let array = make_array storage.fd ~pos:storage.pos storage.mode (Index.to_uint32 mapped_length) in
      storage.array            <- array;
      storage.current_length   <- h.Header.last_next_index;
      storage.mapped_length    <- mapped_length;
      storage.last_root_index  <- h.Header.last_root_index

let get_version t = t.version

let make_buf storage i = get_cell storage i
let make_buf2 storage i = get_bytes storage i 64

let sync t =
  match t.mode with
  | Writer | Private -> ()
  | Reader -> reopen t

module Chunk = struct

  (* Store data bigger than 32 bytes *)

  let ncells size = (size + 8 + 31) / 32

  let get_footer_fields storage last_index =
    let buf = make_buf storage last_index in
    let cdr = Index.of_uint32 @@ C.get_uint32 buf 28 in
    let size = C.get_uint16 buf 26 in
    (cdr, size)

  let get_chunk storage last_index =
    let cdr, size = get_footer_fields storage last_index in
    let ncells = ncells size in
    let first_index = Index.(last_index - of_int ncells + one) in
    (get_bytes storage first_index size, size, cdr)

  let get_chunks storage last_index =
    let rec aux (bufs, size) last_index =
      let buf, bytes, cdr = get_chunk storage last_index in
      let bufs = buf :: bufs in
      let size = size + bytes in (* overflow in 32bit? *)
      if cdr = Index.zero then (bufs, size)
      else aux (bufs, size) cdr
    in
    aux ([], 0) last_index

  let string_of_cstructs bufs = 
    String.concat "" @@ List.map C.to_string bufs

  let read t i = string_of_cstructs @@ fst @@ get_chunks t i
      
  let write_to_chunk storage cdr s off len =
    if storage.mode = Reader then invalid_arg "Reader cannot write";
    assert (String.length s >= off + len);
    let ncells = ncells len in
    let cdr_pos = ncells * 32 - 4 in
    let size_pos = cdr_pos - 2 in

    let i = new_indices storage ncells in
    let last_index = Index.(i + of_int ncells - one) in
    let chunk = get_bytes storage i (32 * ncells) in

    C.blit_from_string s off chunk 0 len;
    C.set_uint16 chunk size_pos len;
    C.set_uint32 chunk cdr_pos (Index.to_uint32 cdr);
    last_index

  let write storage ?(max_cells_per_chunk=1000) s =
    if storage.mode = Reader then invalid_arg "Reader cannot write";
    let max_bytes_per_chunk = 32 * max_cells_per_chunk - 6 in
    let rec f off remain cdr  =
      let len = if remain > max_bytes_per_chunk then max_bytes_per_chunk else remain in
      let cdr' = write_to_chunk storage cdr s off len in
      let off' = off + len in
      let remain' = remain - len in
      if remain' > 0 then f off' remain' cdr'
      else cdr'
    in
    f 0 (String.length s) Index.zero

  let test_write_read st storage =
    let max_cells_per_chunk = Random.State.int st 246 + 10 in
    let size = Random.State.int st (max_cells_per_chunk * 32) + 32 in
    let s = String.init size @@ fun i -> Char.chr (Char.code 'A' + i mod 20) in
    let i = write storage ~max_cells_per_chunk s in
    let s' = string_of_cstructs @@ fst @@ get_chunks storage i in
    assert (s = s')
end
