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
(** { 1  Fixed size storage on a file }

    The Storage has fixed 32-byte size cells.
*)

(** { 2 Types } *)

type mode = 
  | Private (** Private writer.  The file is never modified.  
                Modification to files by other writers may cause unspecified 
                read result of Private. *)
  | Reader  (** Reader *)
  | Writer  (** Writer.  Only one writer can exist. *)

type storage
type t = storage
(** The type *)

(** { 2 Open, close, and commit } *)

val create : 
  ?pos:int64
  -> ?length:int
  -> ?resize_step: Index.t
  -> string
  -> t
(** Create a new storage 

    * pos : Where to start the storage in the file, in bytes.
    * length: The initial size of the storage, excluding [pos].
    * resize_step: How many cells allocated for each resize
    * string : The path name
*)

val open_ : 
  ?pos:int64 
  -> ?resize_step:Index.t 
  -> mode:mode 
  -> string 
  -> t
(** Open an existing storage *)

val truncate : ?length: int -> t -> unit
(** Truncate the data file and reinitialize it.  All the contents are lost.
    [length] is the initial reserved size of the reinitialized file.
*)

val sync : t -> unit
(** For reader to update the storage to catch up the update by the writer 
    process.  For Writer and Private, it does nothing. *)

val close : t -> unit
(** Close the storage *)

val commit : t -> unit
(** Write the current state of the storage at its header.
    If the system crashes, any updates to the storage after 
    the last commit will be lost, even if they are written to
    the file.
*)

(** { 2 Accessor } *)

val filename : t -> string
(** Return the file name *)

val mode : t -> mode
(** Return the opening mode *)

val get_version : t -> int
(** Get the file version *)

val get_last_root_index  : t -> Index.t option
val get_current_length   : t -> Index.t
(** Get the status of the storage 
    
    For Reader, it only returns the lastest information it knows in memory.
    Writer may already update this information on the disk.
*)

val size : t -> Int64.t
(** In bytes *)

val set_last_root_index  : t -> Index.t option -> unit
(** Set the last index of root hash.
    Note that the data are only saved to the file when [Header.commit]
    is called. 
*)

(** { 2 Read and write } *)

val get_cell : t -> Index.t -> Cstruct.t
(** Get the content of the cell specified by the index *)

val get_bytes : t -> Index.t -> int -> Cstruct.t
(** Get the contiguous bytes from the head of the index *)

val new_index : t -> Index.t
(** Allocate a cell and returns its index *)

val new_indices : t -> int -> Index.t
(** Allocate cells and return the first index *)

val make_buf : t -> Index.t -> Cstruct.t
(** make a writable buffer of the cell of the given index *)

val make_buf2 : t -> Index.t -> Cstruct.t      
(** make a 64 bytes writable buffer from the beginning of 
    the cell of the given index *)

module Chunk : sig
  (** Bigger data than a cell *)

  val read : t -> Index.t -> string
  (** Read a big data which is stored from the specified index *)

  val write : t -> ?max_cells_per_chunk:int -> string -> Index.t
  (** Write a big data and returns the index *)
                                                           
  val test_write_read : Random.State.t -> t -> unit
  (** A test function *)
end
