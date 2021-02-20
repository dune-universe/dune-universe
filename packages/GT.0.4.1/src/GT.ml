(**************************************************************************
 *  Copyright (C) 2012-2015
 *  Dmitri Boulytchev (dboulytchev@math.spbu.ru), St.Petersburg State University
 *  Universitetskii pr., 28, St.Petersburg, 198504, RUSSIA
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA
 *
 *  See the GNU Lesser General Public License version 2.1 for more details
 *  (enclosed in the file COPYING).
 **************************************************************************)

(** Implementation of transformation for standart types *)

open Printf

module Format = struct
  include Format
  let pp_print_unit  fmt () = pp_print_string fmt "()"
  let pp_print_int32 fmt n  = Format.pp_print_string fmt @@ Int32.format "%d" n
  let pp_print_int64 fmt n  = Format.pp_print_string fmt @@ Int64.format "%d" n
  let pp_print_nativeint fmt n = Format.pp_print_string fmt @@ Nativeint.format "%d" n
  let pp_print_string fmt s = fprintf fmt "\"%s\"" s
end

type ('a, 'b, 'c) t = {gcata : 'a; plugins : 'b; fix: 'c }

let transform_gc gcata make_obj inh subj =
  let rec obj = lazy (make_obj fself)
  and fself inh x = gcata (Lazy.force obj) inh x in
  fself inh subj

let transform  bundle = transform_gc bundle.gcata

let lift f _ = f
let id x  = x

type comparison = LT | EQ | GT

let chain_compare x f =
  match x with
  | EQ -> f ()
  | _  -> x

let compare_primitive x y =
  if x < y
  then LT
  else if x > y
       then GT
       else EQ

let cmp_to_int x =
  match x with
  | LT -> (-1)
  | GT -> 1
  | EQ -> 0

let poly_tag x =
  let x = Obj.magic x in
  (Obj.magic (if Obj.is_block x then Obj.field x 0 else x) : int)

let vari_tag x =
  if Obj.is_block x then Obj.tag x else Obj.magic x

let compare_poly x y =
  compare_primitive (poly_tag x) (poly_tag y)

let compare_vari x y =
  let x, y = Obj.repr x, Obj.repr y in
  let b = Obj.is_block x in
  (* TODO: rewrite with built-in structural equality *)
  match compare_primitive b (Obj.is_block y) with
  | EQ -> compare_primitive (vari_tag x) (vari_tag y)
  | _ when b -> GT (* block is greater then non-block *)
  | _ -> LT

let string_of_string  s = "\"" ^ String.escaped s ^ "\""
let string_of_unit    _ = "()"
let string_of_char    c = String.make 1 c
let string_of_int32     = Int32.to_string
let string_of_int64     = Int64.to_string
let string_of_nativeint = Nativeint.to_string

GENERIFY(bool)
GENERIFY(int)
GENERIFY(string)
GENERIFY(float)
GENERIFY(char)
GENERIFY(unit)
GENERIFY(int32)
GENERIFY(int64)
GENERIFY(nativeint)

(* Fixpoint combinator to define recursive transformation without extra
 * object allocations *)
let fix0 f t =
  let knot = ref (fun _ -> assert false) in
  let recurse t = f !knot t in
  knot := recurse;
  recurse t

(** {1 List } *)

(* ************************************************************************* *)
(** Standart types go there *)
type 'a plist      = 'a list
type 'a list       = 'a plist

class virtual ['ia, 'a, 'sa, 'inh, 'self, 'syn] list_t =
  object
    method virtual c_Nil  : 'inh -> 'self -> 'syn
    method virtual c_Cons : 'inh -> 'self -> 'a -> 'a list -> 'syn
  end

let gcata_list tr inh s = match s with
| []    -> tr#c_Nil  inh s
| x::xs -> tr#c_Cons inh s x xs

class ['a, 'self] html_list_t fa fself =
  object
    inherit [unit, 'a, HTML.viewer, unit, 'self, HTML.viewer] @list
    method c_Nil  _ _      = View.string "[]"
    method c_Cons _ _ x xs =
      HTML.seq (
         [HTML.string "list"; HTML.ul @@ HTML.seq (List.map (fun x -> HTML.li @@ fa () x) (x::xs))]
      )
(*      View.concat (fa x) (match xs with [] -> View.empty | xs -> HTML.li (fself () xs)) *)
  end

class ['a, 'self] show_list_t fa fself =
  object
    inherit [unit, 'a, string, unit, 'self, string] list_t
    method c_Nil  _ _      = ""
    method c_Cons _ _ x xs = (fa () x) ^ (match xs with [] -> "" | _ -> "; " ^ (fself () xs))
  end

class ['a, 'self] fmt_list_t fa fself =
  object
    inherit ['inh, 'a, unit, 'inh, 'self, unit] list_t
    constraint 'inh = Format.formatter
    method c_Nil  fmt _       =
      Format.fprintf fmt "[]"
    method c_Cons fmt xs _ _  =
      Format.fprintf fmt "@[@,["; (* Extra break here to prevent clashing with m4 macro begin *)
      let () = match xs with
         | [] -> ()
         | x::xs ->
            Format.fprintf fmt "@[ %a@]" fa x;
            List.iter (Format.fprintf fmt "@[; %a@]" fa) xs;
      in
      Format.fprintf fmt "]@]"
  end

class ['a, 'sa, 'self, 'syn ] gmap_list_t fa fself =
  object
    constraint 'syn = 'sa list
    inherit [unit, 'a, 'sa, unit, 'self, 'syn] list_t
    method c_Nil  _ _      = []
    method c_Cons _ _ x xs = (fa () x) :: (fself () xs)
  end
class ['a, 'sa, 'self, 'syn, 'env ] eval_list_t fa fself =
  object
    inherit ['env, 'a, 'sa, 'env, 'self, 'sa list] list_t
    method c_Nil  _   _      = []
    method c_Cons env _ x xs = (fa env x) :: (fself env xs)
  end
class ['a, 'sa, 'self, 'syn, 'env ] stateful_list_t fa fself =
  object
    inherit ['env, 'a, 'env * 'sa, 'env, 'self, 'env * 'sa list] list_t
    method c_Nil  env  _       = (env, [])
    method c_Cons env0 _ x xs : 'env * 'sa list =
      let env1,h  = fa    env0 x  in
      let env2,tl = fself env1 xs in
      env2, (h::tl)
  end

class ['a, 'syn, 'self] foldl_list_t fa fself =
  object
    inherit ['syn, 'a, 'syn, 'syn, 'self, 'syn] list_t
    method c_Nil  s _      = s
    method c_Cons s _ x xs = fself  (fa s x) xs
  end

class ['a, 'syn, 'self] foldr_list_t fa fself =
  object
    inherit ['a, 'syn, 'self] foldl_list_t fa fself
    method! c_Cons s _ x xs = fa (fself s xs) x
  end

class ['a, 'self] eq_list_t fa fself =
  object
    inherit ['a, 'a, bool, 'a list, 'self, bool] list_t
    method c_Nil inh  _      = (inh = [])
    method c_Cons inh _ x xs =
      match inh with
      | y::ys -> fa y x && fself ys xs
      | _ -> false
  end

class ['a, 'self] compare_list_t fa fself =
  object
    inherit ['a, 'a, comparison, 'a list, 'self, comparison] list_t
    method c_Nil inh _ =
      match inh with
      | [] -> EQ
      |  _ -> GT
    method c_Cons inh _ x xs =
      match inh with
      | [] -> LT
      | (y::ys) -> (match fa y x with
                   | EQ -> fself ys xs
                   | c  -> c
                   )
  end


let list :
  ( ('ia, 'a, 'sa, 'inh, _, 'syn) #list_t -> 'inh -> 'a list -> 'syn
  , < show    : ('a -> string)      -> 'a list -> string;
      html    : ('a -> HTML.viewer) -> 'a list -> HTML.viewer;
      gmap    : ('a -> 'b)          -> 'a list -> 'b list;

      fmt     : (Format.formatter -> 'a -> unit) ->
                Format.formatter -> 'a list -> unit;
      eval    : ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list;
      stateful: ('env -> 'a -> 'env * 'b) -> 'env -> 'a list -> 'env * 'b list;
      foldl   : ('c -> 'a -> 'c) -> 'c -> 'a list -> 'c;
      foldr   : ('c -> 'a -> 'c) -> 'c -> 'a list -> 'c;
      eq      : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool;
      compare : ('a -> 'a -> comparison) -> 'a list -> 'a list -> comparison;
    >
  , ( ('inh -> 'a list -> 'syn) ->
          ('ia, 'a, 'sa, 'inh, 'a list, 'syn) list_t  ) ->
       'inh -> 'a list -> 'syn
  ) t =

  {gcata   = gcata_list;
   fix = (fun c -> transform_gc gcata_list c);
   plugins = object
               method show fa l =
                 sprintf "[%a]" (transform_gc gcata_list (new show_list_t (lift fa))) l
               method html    fa   = transform_gc gcata_list (new html_list_t (lift fa)) ()
               method gmap    fa   = transform_gc gcata_list (new gmap_list_t (lift fa)) ()

               method fmt fa inh l =
                  (transform_gc gcata_list (new fmt_list_t fa)) inh l

               method stateful fa  = transform_gc gcata_list (new stateful_list_t fa)
               method eval     fa  = transform_gc gcata_list (new eval_list_t fa)
               method eq       fa  = transform_gc gcata_list (new eq_list_t fa)
               method compare  fa  = transform_gc gcata_list (new compare_list_t fa)
               method foldl    fa  = transform_gc gcata_list (new foldl_list_t fa)
               method foldr    fa  = transform_gc gcata_list (new foldr_list_t fa)
             end
  }


(** {1 Lazy values } *)

module Lazy =
  struct

    type ('a, 'b, 'c) t' = ('a, 'b, 'c) t

    include Lazy

    class virtual ['ia, 'a, 'sa, 'inh, 'self, 'syn] t_t = object
        method virtual t_t : 'inh -> 'a t -> 'syn
      end
    let gcata_t tr inh subj = tr#t_t inh subj
    let gcata_lazy = gcata_t

    class ['a, 'self ] show_t_t fa _fself =
      object
        inherit [unit, 'a, string, unit, 'self, string ] t_t
        method t_t inh subj = fa () @@ Lazy.force subj
      end

    class ['a, 'self ] html_t_t fa _fself =
      object
        inherit [unit, 'a, HTML.viewer, unit, 'self, HTML.viewer ] t_t
        method t_t inh subj = fa () @@ Lazy.force subj
      end

    class ['a, 'sa, 'self, 'syn ] gmap_t_t fa _fself =
      object
        constraint 'syn = 'sa t
        inherit [unit, 'a, 'sa, unit, 'self, 'syn ] t_t
        method t_t inh subj = lazy (fa () @@ Lazy.force subj)
      end

    class ['a, 'sa, 'self, 'syn, 'env ] eval_t_t fa _fself =
      object
        constraint 'syn = 'sa t
        inherit ['env, 'a, 'sa, 'env, 'self, 'syn ] t_t
        method t_t env subj = lazy (fa env @@ Lazy.force subj)
      end

    class ['a, 'sa, 'self, 'syn, 'env ] stateful_t_t fa _fself =
      object
        constraint 'syn = 'sa t
        inherit ['env, 'a, 'sa, 'env, 'self, 'env * 'syn ] t_t
        method t_t env subj =
          let (env1, r) = fa env @@ Lazy.force subj
          in env1, Lazy.from_fun (fun () -> r)
          (* THE SAME AS eval *)
      end

    class ['a, 'syn, 'self ] foldl_t_t fa _fself =
      object
        inherit ['syn, 'a, 'syn, 'syn, 'self, 'syn ] t_t
        method t_t inh subj = fa inh @@ Lazy.force subj
      end

    class ['a, 'syn, 'self ] foldr_t_t fa fself =
      object
        inherit ['a, 'syn, 'self ] foldl_t_t fself fa
      end

    class ['a, 'self ] eq_t_t fa _fself =
      object
        inherit ['a, 'a, bool, 'a t, 'self, bool ] t_t
        method t_t inh subj = fa (Lazy.force inh) (Lazy.force subj)
      end

    class ['a, 'self ] compare_t_t fa _fself =
      object
        inherit ['a, 'a, comparison, 'a t, 'self, comparison ] t_t
        method t_t inh subj = fa (Lazy.force inh) (Lazy.force subj)
      end

    let t : ( ('ia, 'a, 'sa, 'inh, _, 'syn) #t_t -> 'inh -> 'a t -> 'syn,
             < show    : ('a -> string)      -> 'a t -> string;
               html    : ('a -> HTML.viewer) -> 'a t -> HTML.viewer;
               gmap    : ('a -> 'b)          -> 'a t -> 'b t;

               eval    : ('env -> 'a -> 'b) -> 'env -> 'a t -> 'b t;
               stateful: ('env -> 'a -> 'env * 'b) -> 'env -> 'a t -> 'env * 'b t;
               foldl   : ('c -> 'a -> 'c) -> 'c -> 'a t -> 'c;
               foldr   : ('c -> 'a -> 'c) -> 'c -> 'a t -> 'c;
               eq      : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool;
               compare : ('a -> 'a -> comparison) -> 'a t -> 'a t -> comparison;
             >, _) t' =
      let fself _ _ = assert false in
      {gcata   = gcata_lazy;
       fix     = (fun c -> transform_gc gcata_lazy c);
       plugins = object
                   method show     fa  = gcata_lazy (new show_t_t fself (lift fa)) ()
                   method html     fa  = gcata_lazy (new html_t_t fself (lift fa)) ()
                   method gmap     fa  = gcata_lazy (new gmap_t_t fself (lift fa)) ()

                   method eval     fa  = gcata_lazy (new eval_t_t fself fa)
                   method stateful fa  = gcata_lazy (new stateful_t_t fself fa)
                   method eq      fa   = gcata_lazy (new eq_t_t fself fa)
                   method compare fa   = gcata_lazy (new compare_t_t fself fa)
                   method foldl   fa   = gcata_lazy (new foldl_t_t fself fa)
                   method foldr   fa   = gcata_lazy (new foldr_t_t fself fa)
                 end
      }
  end

(** {1 Option } *)
(* ************************************************************************* *)
type 'a poption = 'a option
type 'a option = 'a poption

class virtual ['ia, 'a, 'sa, 'inh, 'self, 'syn] option_t =
  object
    method virtual c_None :   'inh -> 'a option       -> 'syn
    method virtual c_Some :   'inh -> 'a option -> 'a -> 'syn
  end

let gcata_option tr inh subj =
  match subj with
  | None   -> tr#c_None inh subj
  | Some x -> tr#c_Some inh subj x

class ['a, 'self] show_option_t fa _fself =
  object
    inherit [ unit, 'a, string, unit, 'self, string] option_t
    method c_None () _   = "None"
    method c_Some () _ x = Printf.sprintf "Some (%a)" fa x
  end
class ['a, 'self] html_option_t fa _fself =
  object
    inherit [unit, 'a, HTML.viewer, unit, 'self, HTML.viewer] option_t
    method c_None () _   = HTML.string "None"
    method c_Some () _ x = View.concat (HTML.string "Some") (HTML.ul (fa () x))
  end

class ['a, 'self] fmt_option_t fa _fself =
  object
    inherit [ Format.formatter, 'a, unit, Format.formatter, 'self, unit] option_t
    method c_None fmt _   = Format.fprintf fmt "None"
    method c_Some fmt _ x = Format.fprintf fmt "Some (%a)" fa x
  end

class ['a, 'sa, 'self, 'syn ] gmap_option_t fa _fself =
  object
    constraint 'syn = 'sa option
    inherit [unit, 'a, 'sa, unit, 'self, 'syn ] option_t
    method c_None () _ = None
    method c_Some () _ x = Some (fa () x)
  end

class ['a, 'sa, 'self, 'syn, 'env ] eval_option_t fa _fself =
  object
    inherit ['env, 'a, 'env * 'sa, 'env, 'self, 'sa option] option_t
    method c_None _   _   = None
    method c_Some env _ x = Some (fa env x)
  end

class ['a, 'sa, 'self, 'syn, 'env ] stateful_option_t fa _fself =
  object
    constraint 'syn = 'sa option
    inherit ['env, 'a, 'sa, 'env, 'self, 'env * 'syn ] option_t
    method c_None env _   = (env,None)
    method c_Some env _ x =
      let env1,r = fa env x in
      (env1, Some r)
  end

class ['a, 'syn, 'self] foldl_option_t fa _fself =
  object
    inherit ['syn, 'a, 'syn, 'syn, 'self, 'syn] option_t
    method c_None s _   = s
    method c_Some s _ x = fa s x
  end

class ['a, 'syn, 'self] foldr_option_t fa _fself =
  object
    inherit ['a, 'syn, 'self] foldl_option_t fa _fself
  end

class ['a, 'self] eq_option_t fa _fself =
  object
    inherit ['a, 'a, bool, 'a option, 'self, bool] option_t
    method c_None inh _   = (inh = None)
    method c_Some inh _ x =
      match inh with
      | Some y -> fa y x
      | _ -> false
  end

class ['a, 'self] compare_option_t fa _fself =
  object
    inherit ['a, 'a, comparison, 'a option, 'self, comparison] option_t
    method c_None inh _ = match inh with
      | None -> EQ
      | _  -> GT
    method c_Some inh _ x =
      match inh with
      | None -> LT
      | Some y -> fa y x
  end

let option : ( ('ia, 'a, 'sa, 'inh, _, 'syn) #option_t -> 'inh -> 'a option -> 'syn,
              < show    : ('a -> string)      -> 'a option -> string;
                html    : ('a -> HTML.viewer) -> 'a option -> HTML.viewer;
                gmap    : ('a -> 'b)          -> 'a option -> 'b option;

                fmt     : (Format.formatter -> 'a -> unit) ->
                          Format.formatter -> 'a option -> unit;
                stateful: ('env -> 'a -> 'env * 'b) -> 'env -> 'a option -> 'env * 'b option;
                eval    : ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option;
                foldl   : ('c -> 'a -> 'c) -> 'c -> 'a option -> 'c;
                foldr   : ('c -> 'a -> 'c) -> 'c -> 'a option -> 'c;
                eq      : ('a -> 'a -> bool) -> 'a option -> 'a option -> bool;
                compare : ('a -> 'a -> comparison) -> 'a option -> 'a option -> comparison;
              >, _) t =
  {gcata   = gcata_option;
   fix = (fun c -> transform_gc gcata_option c);
   plugins = object
               method show     fa = transform_gc gcata_option (new show_option_t (lift fa)) ()
               method html     fa = transform_gc gcata_option (new html_option_t (lift fa)) ()
               method gmap     fa = transform_gc gcata_option (new gmap_option_t (lift fa)) ()

               method fmt      fa = transform_gc gcata_option (new fmt_option_t fa)
               method stateful fa = transform_gc gcata_option (new stateful_option_t fa)
               method eval     fa = transform_gc gcata_option (new eval_option_t fa)
               method eq       fa = transform_gc gcata_option (new eq_option_t fa)
               method compare  fa = transform_gc gcata_option (new compare_option_t fa)
               method foldl    fa = transform_gc gcata_option (new foldl_option_t fa)
               method foldr    fa = transform_gc gcata_option (new foldr_option_t fa)
             end
  }


(* Pairs and other stuff without explicit structure *)
(*******************************************************************************)
(** Arrow *)
type ('a, 'b) arrow = 'a -> 'b

let gcata_arrow tr inh arr = tr#c_Arrow inh arr

class virtual ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, 'self, 'syn] arrow_t =
  object
    method virtual c_Arrow : 'inh -> ('a,'b) arrow -> 'syn
  end

class ['a, 'b, 'self] show_arrow_t fa fb _ =
  object
    inherit [unit, 'a, string, unit, 'b, string, unit, 'self, string] arrow_t
    method c_Arrow () _ = Printf.sprintf "<function>"
  end

class ['a, 'b, 'self] fmt_arrow_t fa fb _ =
  object
    inherit ['inh, 'a, unit, 'inh, 'b, unit, 'inh, 'self, unit] arrow_t
    constraint 'inh = Format.formatter
    method c_Arrow fmt _ = Format.fprintf fmt "<function>"
  end

class ['a, 'b, 'self] html_arrow_t fa fb _ =
  object
    inherit [unit, 'a, 'syn, unit, 'b, 'syn, unit, 'self, 'syn] arrow_t
    constraint 'syn = HTML.viewer
    method c_Arrow () _ = HTML.string "<arrow>"
  end

class ['a, 'sa, 'b, 'sb, 'self] gmap_arrow_t (fa: unit -> 'a -> 'sa) fb _ =
  object
    inherit [unit, 'a, 'sa, unit, 'b, 'sb, unit, 'self, ('sa, 'sb) arrow] arrow_t
    method c_Arrow () _ = failwith "gmap for arrows is not implemented"
  end

class ['a, 'sa, 'b, 'sb, 'env, 'self] eval_arrow_t fa fb _ =
  object
    inherit ['env, 'a, 'sa, 'env, 'b, 'sb, 'env, 'self, ('sa, 'sb) arrow] arrow_t
    method c_Arrow _ _ = failwith "eval for arrows is not implemented"
  end

class ['a, 'sa, 'b, 'sb, 'self, 'syn, 'env ] stateful_arrow_t fa fb _ =
  object
    inherit ['env, 'a, 'env * 'sa, 'env, 'b, 'sb, 'env, 'self, 'env * ('sa, 'sb) arrow] arrow_t
    method c_Arrow _ _ = failwith "stateful for arrows is not implemented"
  end

class ['a, 'b, 'syn, 'self] foldl_arrow_t fa fb _ =
  object
    inherit ['syn, 'a, 'syn, 'syn, 'b, 'syn, 'syn, 'self, 'syn] arrow_t
    method c_Arrow _ _ = failwith "foldl for arrows is not implemented"
  end

class ['a, 'b, 'syn, 'self] foldr_arrow_t fa fb _ =
  object
    inherit ['syn, 'a, 'syn, 'syn, 'b, 'syn, 'syn, 'self, 'syn] arrow_t
    method c_Arrow _ _ = failwith "foldr for arrows is not implemented"
  end

class ['a, 'b, 'self] eq_arrow_t fa fb _ =
  object
    inherit ['a, 'a, bool, 'b, 'b, bool, ('a, 'b) arrow, 'self, bool] arrow_t
    method c_Arrow _ _ = failwith "eq for arrows is not implemented"
  end

class ['a, 'b, 'self] compare_arrow_t fa fb _ =
  object
    inherit ['a, 'a, 'syn, 'b, 'b, 'syn, ('a, 'b) arrow, 'self, 'syn] arrow_t
    constraint 'syn = comparison
    method c_Arrow _ _ = failwith "compare for arrows is not implemented"
  end

let arrow:
  ( ('ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, _, 'syn) #arrow_t -> 'inh -> ('a, 'b) arrow -> 'syn,
              < show    : ('a -> string) -> ('b -> string) ->
                          ('a, 'b) arrow -> string;
                html    : ('a -> HTML.viewer) -> ('b -> HTML.viewer) ->
                          ('a, 'b) arrow -> HTML.viewer;
                gmap    : ('a -> 'c) -> ('b -> 'd) ->
                          ('a, 'b) arrow -> ('c, 'd) arrow;

                fmt     : (Format.formatter -> 'a -> unit) ->
                          (Format.formatter -> 'b -> unit) ->
                          Format.formatter -> ('a,'b) arrow -> unit;
                stateful: ('env -> 'a -> 'env * 'c) ->
                          ('env -> 'b -> 'env * 'd) ->
                          'env -> ('a, 'b) arrow -> 'env * ('c, 'd) arrow;
                eval    : ('env -> 'a -> 'c) -> ('env -> 'b -> 'd) ->
                          'env -> ('a, 'b) arrow -> ('c, 'd) arrow;
                foldl   : ('c -> 'a -> 'c) -> ('c -> 'b -> 'c) -> 'c -> ('a, 'b) arrow -> 'c;
                foldr   : ('c -> 'a -> 'c) -> ('c -> 'b -> 'c) -> 'c -> ('a, 'b) arrow -> 'c;
                eq      : ('a -> 'a -> bool) -> ('b -> 'b -> bool) ->
                          ('a, 'b) arrow -> ('a, 'b) arrow -> bool;
                compare : ('a -> 'a -> comparison) -> ('b -> 'b -> comparison) ->
                          ('a, 'b) arrow -> ('a, 'b) arrow -> comparison;
              >,_) t =
  {gcata   = gcata_arrow;
   fix = (fun c -> transform_gc gcata_arrow c);
   plugins =
     let tr  obj subj     = transform_gc gcata_arrow obj ()  subj in
     let tr1 obj inh subj = transform_gc gcata_arrow obj inh subj in
     object
       method show     fa fb = tr  (new show_arrow_t (lift fa) (lift fb))
       method html     fa fb = tr  (new html_arrow_t (lift fa) (lift fb))
       method gmap     fa fb = tr  (new gmap_arrow_t (lift fa) (lift fb))

       method fmt      fa fb = tr1 (new fmt_arrow_t  fa fb)
       method eval     fa fb = tr1 (new eval_arrow_t fa fb)
       method stateful fa fb = tr1 (new stateful_arrow_t fa fb)
       method eq       fa fb = tr1 (new eq_arrow_t   fa fb)
       method compare  fa fb = tr1 (new compare_arrow_t fa fb)
       method foldl    fa fb = tr1 (new foldl_arrow_t fa fb)
       method foldr    fa fb = tr1 (new foldr_arrow_t fa fb)
  end
 }

(*******************************************************************************)

(****************************************************************************)
(* {1 Mutable references} *)
type 'a ref2 = 'a ref
type 'a ref = 'a ref2
class virtual ['ia,'a,'sa, 'inh, 'e, 'syn] ref_t =
object
  method virtual c_ref : 'inh -> 'a -> 'syn
end
let gcata_ref tr inh r = tr#c_ref inh !r

class ['a, 'self] fmt_ref_t fa _ =
  object
    inherit [ 'inh, 'a, unit
            , 'inh, 'self, unit] ref_t
    constraint 'inh = Format.formatter
    method c_ref fmt a =
      Format.fprintf fmt "!(%a)" fa a
  end
class ['a, 'self] html_ref_t fa _ =
  object
    inherit [ 'inh, 'a, 'syn
            , 'inh, 'self, 'syn] ref_t
    constraint 'syn = HTML.viewer
    constraint 'inh = unit
    method c_ref () a = fa () a
  end
class ['a, 'self] show_ref_t fa _ =
  object
    inherit [ 'inh, 'a, 'syn
            , 'inh, 'self, 'syn] ref_t
    constraint 'syn = string
    constraint 'inh = unit
    method c_ref () a = fa () a
  end

let ref:
    ( ('ia, 'a, 'sa, 'inh, _, 'syn ) #ref_t ->
      'inh -> 'a ref -> 'syn
    , < html    : ('a -> HTML.er) ->  'a ref -> HTML.er;
        show    : ('a -> string)  ->  'a ref -> string;

        fmt     : (Format.formatter -> 'a -> unit) ->
                  Format.formatter -> 'a ref -> unit;
      >,_) t =
  {gcata   = gcata_ref;
   fix = (fun c -> transform_gc gcata_ref c);
   plugins = object
     method show    fa = transform_gc gcata_ref (new show_ref_t (lift fa)) ()
     method html    fa = transform_gc gcata_ref (new html_ref_t (lift fa)) ()

     method fmt     fa = transform_gc gcata_ref (new fmt_ref_t  fa)
  end
}
(*** arrays *****************************************************************)
(* TODO: array are not really implemented *)
(* {1 Arrays (N.B. WIP) } *)
type 'a parray      = 'a array
type 'a array       = 'a parray

class virtual ['ia, 'a, 'sa, 'inh, 'self, 'syn] array_t = object
  method virtual do_array  : 'inh -> 'a array -> 'syn
end

let gcata_array tr inh subj = tr#do_array inh subj

class ['a, 'self] show_array_t fa fself = object
  inherit [unit, 'a, string, unit, 'self, string] array_t
  method do_array () arr =
    "[|" ^ (Array.fold_right
              (fun x s -> Printf.sprintf "%a; %s" fa x s) arr " |]")
end

class ['a, 'sa, 'self, 'syn] gmap_array_t fa fself =
  object
    inherit [unit, 'a, 'sa, unit, 'self, 'syn] array_t
    constraint 'syn = 'sa array
    method do_array () arr = Array.map (fa ()) arr
  end
class ['a, 'self] html_array_t fa fself =
  object
    inherit [unit, 'a, HTML.viewer, unit, 'self, HTML.viewer] array_t
    method do_array () arr =
      HTML.ul @@ HTML.seq (
        [ HTML.string "array" ] @ List.map (fun x -> HTML.li @@ fa () x) @@ Array.to_list arr
        )
  end

class ['a, 'self] fmt_array_t fa fself = object
  inherit [Format.formatter, 'a, unit, Format.formatter, 'self, unit] array_t

  method do_array fmt arr =
    Format.fprintf fmt "[| ";
    Array.iter (fun x -> Format.fprintf fmt "%a; " fa x) arr;
    Format.fprintf fmt " |]"
end

class ['a, 'sa, 'self, 'syn, 'env ] eval_array_t fa fself =
  object
    inherit ['env, 'a, 'sa, 'env, 'self, 'syn ] array_t
    constraint 'syn = 'sa array
    method do_array env arr = Array.map (fa env) arr
  end
class ['a, 'sa, 'self, 'syn, 'env ] stateful_array_t fa fself =
  object
    inherit ['env, 'a, 'env * 'sa, 'env, 'self, 'env * 'sa array] array_t
    method do_array env0 arr =
      let n = Array.length arr in
      if n = 0 then ([||], env0)
      else
        let (x1,env1) = fa env0 (Array.get arr 0) in
        let env = Stdlib.ref env1 in
        let ans = Array.make n x1 in
        for i=1 to n - 1 do
          let (x,env2) = fa !env (Array.get arr i) in
          env := env2;
          Array.set ans i x
        done;
        (!env, ans)
  end

class ['a, 'syn, 'self] foldl_array_t fa fself =
  object
    inherit ['syn, 'a, 'syn, 'syn, 'self, 'syn] array_t
    method do_array env arr = Array.fold_left fa env arr
  end

class ['a, 'syn, 'self] foldr_array_t fa fself =
  object
    inherit ['syn, 'a, 'syn, 'syn, 'self, 'syn] array_t
    method do_array env arr = Array.fold_right (fun x acc -> fa acc x) arr env
  end

class ['a, 'self] eq_array_t fa fself =
  object
    inherit ['a, 'a, bool, 'a array, 'self, bool] array_t
    method do_array env arr =
      let n = Array.length arr in
      (Array.length env = n) &&
      (let ans = Stdlib.ref true in
       for i=0 to n do
         ans := !ans && (fa (Array.get env i) (Array.get arr i) )
       done;
       !ans
      )
  end

class ['a, 'self] compare_array_t fa fself =
  object
    inherit ['a, 'a, comparison, 'a array, 'self, comparison] array_t
    method do_array env arr =
      let n = Array.length arr in
      if Array.length env < n then LT else
      (let ans = Stdlib.ref EQ in
       for i=0 to n do
         ans := chain_compare !ans (fun () -> fa (Array.get env i) (Array.get arr i))
       done;
       !ans
      )
  end

let array =
  { gcata = gcata_array
  ; fix = (fun c -> transform_gc gcata_array c)
  ; plugins =
      let tr  obj fa   s = transform_gc gcata_array (obj fa) () s in
      let tr1 obj fa i s = transform_gc gcata_array (obj fa)  i s in
      object
        method show fa  = tr (new show_array_t) (lift fa)
        method gmap fa  = tr (new gmap_array_t) (lift fa)
        method html fa  = tr (new html_array_t) (lift fa)

        method fmt      fa = tr1 (new fmt_array_t) fa
        method eval     fa = tr1 (new eval_array_t) fa
        method stateful fa = tr1 (new stateful_array_t) fa
        method compare  fa = tr1 (new compare_array_t) fa
        method eq       fa = tr1 (new eq_array_t) fa
        method foldl    fa = tr1 (new foldl_array_t) fa
        method foldr    fa = tr1 (new foldr_array_t) fa
    end
  }

(*** bytes *****************************************************************)
(* {1 Bytes (mutable string) } *)
type bytes = Bytes.t

class virtual ['inh, 'self, 'syn] bytes_t = object
  method virtual do_bytes  : 'inh -> bytes -> 'syn
end

let gcata_bytes tr inh subj = tr#do_bytes inh subj

class ['self] html_bytes_t fself =
  object
    inherit [unit, 'self, HTML.viewer] bytes_t
    method do_bytes () arr =
      HTML.string @@ Bytes.to_string arr
  end

class ['self] show_bytes_t fself = object
  inherit [ unit, 'self, string] bytes_t
  method do_bytes () = Bytes.to_string
end
class ['self, 'syn] gmap_bytes_t fself = object
  inherit [unit, 'self, 'syn] bytes_t
  constraint 'syn = bytes
  method do_bytes () arr = arr
end

class ['self] fmt_bytes_t fself = object
  inherit [Format.formatter, 'self, unit] bytes_t

  method do_bytes fmt arr =
    Format.fprintf fmt "%S" (Bytes.to_string arr)
end
class [ 'self, 'syn, 'env ] eval_bytes_t fself =
  object
    inherit [ 'env, 'self, 'syn] bytes_t
    constraint 'syn = bytes
    method do_bytes env arr = arr
  end
class [ 'self, 'syn, 'env ] stateful_bytes_t fself =
  object
    inherit ['env, 'self, 'syn] bytes_t
    constraint 'syn = 'env * bytes
    method do_bytes env0 arr = (env0,arr)
  end

class ['syn, 'self] foldl_bytes_t fself =
  object
    inherit ['syn, 'self, 'syn] bytes_t
    method do_bytes env _ = env
  end

class ['syn, 'self] foldr_bytes_t fself =
  object
    inherit ['syn, 'self, 'syn] bytes_t
    method do_bytes env _ = env
  end

class ['self] eq_bytes_t fself =
  object
    inherit [bytes, 'self, bool] bytes_t
    method do_bytes env arr = (Bytes.compare env arr = 0)
  end

class ['self] compare_bytes_t fself =
  object
    inherit [bytes, 'self, comparison] bytes_t
    method do_bytes env arr =
      let c = Bytes.compare env arr in
      if c < 0 then LT
      else if c = 0 then EQ
      else GT
  end

let bytes =
  { gcata = gcata_bytes
  ; fix = (fun c -> transform_gc gcata_bytes c)
  ; plugins =
      let tr  obj    s = gcata_bytes (obj (fun _ _ -> assert false) ) () s in
      let tr1 obj i  s = gcata_bytes (obj (fun _ _ -> assert false) ) i  s in
      object
        method show   = tr (new show_bytes_t)
        method gmap   = tr (new gmap_bytes_t)
        method html   = tr (new html_bytes_t)

        method fmt    = tr1 (new fmt_bytes_t)

        method eval   = tr1 (new eval_bytes_t)
        method stateful = tr1 (new stateful_bytes_t)
        method compare  = tr1 (new compare_bytes_t)
        method eq       = tr1 (new eq_bytes_t)
        method foldl    = tr1 (new foldl_bytes_t)
        method foldr    = tr1 (new foldr_bytes_t)
    end
  }

(****************************************************************************)
let show    t = t.plugins#show
let html    t = t.plugins#html
let gmap    t = t.plugins#gmap

let fmt     t = t.plugins#fmt
let eval    t = t.plugins#eval
let foldl   t = t.plugins#foldl
let foldr   t = t.plugins#foldr
let eq      t = t.plugins#eq
let compare t = t.plugins#compare
let stateful t = t.plugins#stateful
let eval     t = t.plugins#eval
