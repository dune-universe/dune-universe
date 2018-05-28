open Printf

(** {2 Helper funs}
 ***********************************************************************)

(* specialized for speed *)
let min i j = if (i:int) < j then i else j

module List =
struct
  include List

  let rec list_iteri_loop f i = function
    | [] -> ()
    | a :: tl -> f i a; list_iteri_loop f (succ i) tl

  let iteri ~(f: int -> _ -> unit) l = list_iteri_loop f 0 l
end

let option_map f = function Some v -> Some(f v) | None -> None

(** Return a string showing the content of the array *)
let string_of_array a =
  if Array.length a = 0 then "[| |]"
  else begin
    let b = Buffer.create 80 in
    Buffer.add_string b "[|";
    Buffer.add_string b (string_of_int a.(0));
    for i = 1 to Array.length a - 1 do
      Buffer.add_string b "; ";
      Buffer.add_string b (string_of_int a.(i));
    done;
    Buffer.add_string b "|]";
    Buffer.contents b
  end

(** [get_rank default m] returns the length of by the first array in
    the list of options [m]. *)
let rec get_rank default = function
  | [] -> default
  | None :: t -> get_rank default t
  | Some m :: _ -> Array.length m

let get_mat_rank name rank default = function
  | None -> Array.make rank default (* Create matrix with default value *)
  | Some m ->
      if Array.length m <> rank then
        invalid_arg(sprintf "%s: expected length=%i, got=%i"
                      name rank (Array.length m));
      m


(** {2 Geometry checks}
 ***********************************************************************)

(* This module perform some checks on the dimensions and howmany
   specifications that depend on the layout but not on the
   precision.  *)
module Geom = struct
  let rec different_sub ofs1 n1 ofs2 n2 len =
    len > 0 && (n1.(ofs1) <> n2.(ofs2)
               || different_sub (ofs1 + 1) n1 (ofs2 + 1) n2 (len - 1))

  (* The arrays of dimensions are always arranged from the slow
     varying dimension to the fast one.  This the "special" dimension
     is always last. *)
  let r2c ni no =
    let len = Array.length ni in
    len <> Array.length no
    || ni.(len - 1)/2 + 1 <> no.(len - 1)
    || different_sub 0 ni 0 no (len - 2)

  let logical_c2c ni no msg =
    if ni <> no then invalid_arg msg;
    ni

  let logical_r2r = logical_c2c

  let logical_r2c ni no msg =
    if r2c ni no then invalid_arg msg;
    ni

  let logical_c2r ni no msg = logical_r2c no ni msg
end
