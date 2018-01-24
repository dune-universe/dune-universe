open Pcre

module Gen = Regexp_gen

type 'a t = ('a, Pcre.regexp) Gen.t

module Literal = struct

  module OrakudaRegexpInternal = struct
  
    class virtual group = Gen.group
  
    let create string ~flags binder = Gen.create string (Pcre.regexp ~flags string) binder
    
    let make_group_obj rex s substrs =
    
      let subject_start, subject_end = Pcre.get_substring_ofs substrs 0 in (* may raise an exception *)
      let left = String.sub s 0 subject_start in
      let right = String.sub s subject_end (String.length s - subject_end) in
      let groups = Pcre.get_opt_substrings substrs ~full_match:true in
      Gen.make_group_obj rex left groups right

    (* replace *)
    let replace rex ~templ = 
      Pcre.qreplace ~rex:rex.Gen.re ~templ ?pat:None
    
    let replace_first rex ~templ = 
      Pcre.qreplace_first ~rex:rex.Gen.re ~templ ?pat:None 
    
    let substitute_substrings_gen 
          (substf : ?iflags : irflag -> ?flags : rflag list ->
                    ?rex : regexp -> ?pat : string -> ?pos : int ->
                    ?callout : callout -> subst : (substrings -> string) ->
                    string -> string) f rex 
          ?iflags ?flags ?pos ?callout
          s =
      substf ~rex:rex.Gen.re ?pat:None
        ~subst: (fun substrs -> f (make_group_obj rex s substrs))
        ?iflags ?flags ?pos ?callout s
    
    let substitute_substrings f rex = 
      substitute_substrings_gen Pcre.substitute_substrings f rex
    
    let substitute_substrings_first f rex =
      substitute_substrings_gen Pcre.substitute_substrings_first f rex
    

  end

end

open Literal.OrakudaRegexpInternal

let regexp t = t.Gen.re

let exec_exn rex ?iflags ?flags ?pos ?callout s =
  let substrs = Pcre.exec ~rex:rex.Gen.re ?iflags ?flags ?pos ?callout s in
  make_group_obj rex s substrs
;;

let exec rex ?iflags ?flags ?pos ?callout s =
  try
    Some (exec_exn rex ?iflags ?flags ?pos ?callout s)
  with
  | Not_found -> None

(* split *)
let split rex = Pcre.split ~rex:rex.Gen.re ?pat:None

let replace = replace
let replace_first = replace_first
let substitute_substrings = substitute_substrings
let substitute_substrings_first = substitute_substrings_first
  
module Infix = struct
  let (=~) s ?iflags ?flags ?pos ?callout rex = 
    exec rex ?iflags ?flags ?pos ?callout s
  ;;

  let case s = `Error s
  
  let (==>) r f = function
    | `Ok v -> `Ok v
    | `Error s -> 
        match s =~ r with
        | Some v -> `Ok (f v)
        | None -> `Error s
  
  let default f = function
    | `Ok v -> v
    | `Error _ -> f ()
end

let build_case rex f s =
  match exec rex s with
  | Some v -> Some (f v)
  | None -> None

let build_cases ?(default= fun () -> raise Not_found) rex_fs s = 
  let rec with_aux = function
    | [] -> default ()
    | rex_f::rex_fs ->
	match rex_f s with
	| Some v -> v
	| None -> with_aux rex_fs
  in
  with_aux rex_fs
;;
