(** Conversions functions *)

open Ctypes
open Foreign

(** C pointer of null terminated array of C strings *)
type carray_of_strings = char ptr ptr
let carray_of_strings : carray_of_strings typ = ptr (ptr char)

(** Converts C array of strings to OCaml list of strings *)
let carray_of_strings_to_list : char ptr ptr -> string list =
  let rec loop acc p =
    match coerce (ptr char) string_opt !@p with
    | None -> List.rev acc
    | Some s -> loop (s :: acc) (p +@ 1)
in loop []

(** Converts C array of strings to OCaml array of strings *)
external carray_of_strings_to_array : carray_of_strings -> string array =
  "caml_copy_string_array"

(** GList struct *)
type glist
let glist : glist structure typ = structure "GList"
let glist_data  = field glist "data" (ptr void)
let glist_next  = field glist "next" (ptr_opt glist)
let glist_prev  = field glist "prev" (ptr_opt glist)
let () = seal glist

let g_free =
  foreign "g_free"
    (ptr void @-> returning void)

let g_free_t = ptr void @-> returning void

let glist_free_full =
  foreign "g_list_free_full"
    (ptr glist @-> funptr g_free_t @-> returning void)

(** Get the next element of a glist *)
let g_list_next l_ptr =
  getf (!@l_ptr) glist_next

(** Get the void ptr data of the current element *)
let g_list_data l_ptr =
  getf (!@l_ptr) glist_data

(** Transform a GList of strings to an OCaml list of strings *)
let glist_of_strings_to_list glist_ptr =
  let rec loop acc p =
    match p with
    | None -> List.rev acc
    | Some p' -> let data = g_list_data p' in
      let next = g_list_next p' in
      match coerce (ptr void) string_opt data with
      | None -> loop acc next
      | Some s -> loop (s :: acc) next
  in
  let ocaml_list = loop [] (Some glist_ptr) in
  let _ = glist_free_full glist_ptr g_free in
  ocaml_list

(** GSList struct *)
type gslist
let gslist : gslist structure typ = structure "GSList"
let gslist_data  = field gslist "data" (ptr void)
let gslist_next  = field gslist "next" (ptr_opt gslist)
let () = seal gslist

(** Get the next element of a gslist *)
let g_slist_next l_ptr =
  getf (!@l_ptr) gslist_next

(** Get the void ptr data of the current element *)
let g_slist_data l_ptr =
  getf (!@l_ptr) gslist_data

(** Transform a GSList of strings to an OCaml list of strings *)
let gslist_of_strings_to_list gslist_ptr =
  let rec loop acc p =
    match p with
    | None -> List.rev acc
    | Some p' -> let data = g_slist_data p' in
      let next = g_slist_next p' in
      match coerce (ptr void) string_opt data with
      | None -> loop acc next
      | Some s -> loop (s :: acc) next
  in loop [] (Some gslist_ptr)
