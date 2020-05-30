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
module Open = struct
  let from_Some = Option.from_Some

  let to_file ~file:fn s =
    let oc = open_out fn in
    output_string oc s;
    close_out oc

  let with_time f =
    let t1 = Ptime_clock.now () in
    let res = f () in
    let t2 = Ptime_clock.now () in
    res, (Ptime.Span.to_float_s @@ Ptime.diff t2 t1)
  
  let (^/) = Filename.concat

  let failwithf fmt = Printf.ksprintf Stdlib.failwith fmt
end
  
module Exn = struct
  let catch f a = match f a with
    | exception e -> Error (`Exn e)
    | x -> Ok x
  
  let protect f fin = 
    match f () with
    | exception e -> fin (); raise e
    | x -> fin (); x
end

module Format = struct
  include Format

  type 'a t = formatter -> 'a -> unit

  let string = pp_print_string

  let rec list  (sep : (unit, formatter, unit) format) p ppf = function
    | [] -> ()
    | [x] -> p ppf x
    | x::xs ->
        fprintf ppf "@[%a@]%t%a"
          p x
          (fun ppf -> fprintf ppf sep)
          (list sep p) xs

  let option p ppf = function
    | None -> fprintf ppf "None"
    | Some x -> fprintf ppf "Some (%a)" p x
end

module String = struct
  include String

  let find_char p s pos =
    let len = String.length s in
    let rec f pos =
      if len <= pos then None
      else if p @@ String.unsafe_get s pos then Some pos
      else f (pos+1)
    in
    f pos
      
  let split_by_char p s =
    let len = String.length s in
    let rec f rev_list start pos =
      match find_char p s pos with
      | None -> List.rev_map (fun (a,b) -> String.sub s a b) @@ (start, len - start) :: rev_list
      | Some pos' ->
          f ((start, pos' - start) :: rev_list) (pos'+1) (pos'+1)
    in
    f [] 0 0
      
  let () =
    assert (split_by_char (function '/' -> true | _ -> false) "/1/23//456/" = [""; "1"; "23"; ""; "456"; ""]);
    assert (split_by_char (function '/' -> true | _ -> false) "/" = [""; ""]);
    assert (split_by_char (function '/' -> true | _ -> false) "" = [""])

  let for_all f s =
    let len = String.length s in
    let rec aux = function
      | -1 -> true
      | i -> 
          let c = String.unsafe_get s i in
          if f c then aux (i-1) else false
    in
    aux (len - 1)
end

module Hashtbl = struct
  include Hashtbl

  let alter tbl f k =
    match f  @@ Hashtbl.find_opt tbl k with
    | None -> Hashtbl.remove tbl k
    | Some x -> Hashtbl.replace tbl k x
end

module List = struct
  include List

  let split_at n xs =
    let rec split_at_ n st xs =
      if n <= 0 then st, xs
      else match xs with
      | [] -> st, []
      | x::xs -> split_at_ (n-1) (x::st) xs
    in
    let r, dropped = split_at_ n [] xs in
    rev r, dropped
    
  let rec find_map_opt f = function
    | [] -> None
    | x::xs -> 
        match f x with
        | Some y -> Some y
        | None -> find_map_opt f xs

  let rev_filter_map f lst = fold_left (fun st x -> match f x with
      | Some v -> v :: st
      | None -> st) [] lst
  
  (** mapMaybe of Haskell *)
  let filter_map f lst = rev @@ rev_filter_map f lst

  (** Tail recursive verions *)

  let (@) xs ys = rev_append (rev xs) ys
    
  let rev_concat xs = 
    let rec f acc = function
      | [] -> acc
      | x::xs -> f (rev_append x acc) xs
    in
    f [] xs

  let concat xs = rev @@ rev_concat xs

  let map f xs =
    let rec loop acc = function
      | [] -> rev acc
      | x::xs -> loop (f x :: acc) xs
    in
    loop [] xs
      
  let rev_concat_map f xs =
    let rec loop acc = function
      | [] -> acc
      | x::xs -> loop (rev_append (f x) acc) xs
    in
    loop [] xs

  let concat_map f xs = rev @@ rev_concat_map f xs

  let rec is_prefix xs ys = match xs, ys with
    | [], _ -> Some ys
    | x::xs, y::ys when x = y -> is_prefix xs ys
    | _ -> None

  let rev_group_by eq rev_xs =
    let rec grouping gs cur_group = function
      | [] ->
          begin match cur_group with
          | [] -> gs
          | _ -> cur_group :: gs
          end
      | x::xs ->
          match cur_group with
          | [] -> grouping gs [x] xs
          | y::_ ->
              if eq x y then grouping gs (x::cur_group) xs
              else grouping (cur_group::gs) [x] xs
    in
    grouping [] [] rev_xs
  
  let group_by eq xs = rev_group_by eq (rev xs)

  let rev_take n xs =
    let rec loop st n xs =
      if n <= 0 then List.rev st
      else
        match xs with
        | [] -> List.rev st
        | x::xs -> loop (x::st) (n-1) xs
    in
    loop [] n xs
      
  let take_while p xs =
    let rec loop st = function
      | [] -> List.rev st, []
      | x::xs when p x -> loop (x::st) xs
      | xs -> List.rev st, xs
    in
    loop [] xs

  let take_while_map p xs =
    let rec loop st = function
      | [] -> List.rev st, []
      | x::xs ->
          match p x with
          | None -> List.rev st, x::xs
          | Some y -> loop (y::st) xs
    in
    loop [] xs
end 
