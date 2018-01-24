(*
  Regexp creation $/.../
*)

open Ppx_orakuda.Regexp_pcre
open Ppx_orakuda.Regexp_pcre.Infix
open Ppx_orakuda.Regexp_pcre.Literal

(* str_item *)

(* Re_pcre does not support named group :-(
let _ = {m|([0-9]+)(?P<x>[a-z]+)|([A-Z]+)|m};;
*)
let _ = {m|([0-9]+)([a-z]+)|([A-Z]+)|m};;

let _ = {m|([0-9]+)([a-z]+)|([A-Z]+)|m}, 1;;

let rex = [%m "([0-9]+)([a-z]+)|([A-Z]+)" ]

let res =
  match exec rex "abc123def456" with
  | None -> assert false
  | Some res ->
      assert (res#_0 = "123def");
      assert (res#_1 = "123");
      assert (res#_2 = "def");
      (* assert (res#_2 = res#x); *)
      assert (res#_left = "abc");
      assert (res#_right = "456");
      assert (res#_3 = "");
      assert (res#_last = "def");
      res
;;

let _ = assert (("HeLlO" =~ {m|hello/i|m}) <> None)

let _ = assert (("HeLlO" =~ {m|hello|m}) = None)

let () =
  match "http://www.www.com" =~ {m|http:\/\/([^.]+)\.([^.]+)\.([^.]+)|m} with
  | None -> assert false
  | Some res -> 
      assert (res#_1 = "www"
           && res#_2 = "www"
           && res#_3 = "com")

