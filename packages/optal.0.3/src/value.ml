open Common_types

type error =
  | Unbound_variable of string
  | Mutliple_declaration of id
  | Mutliple_declaration_field of id
  | General of (Format.formatter -> unit)
  | Unbound_field of (Format.formatter -> unit)

exception Error of error * Loc.t

let error_gen loc f =
  raise (Error (General (fun ppf -> f ppf),loc))

(* let error loc fmt = *)
(*   Format.ksprintf (fun s -> raise *)
(*       (Error (General (fun ppf -> Format.pp_print_string ppf s),loc))) *)
(*     fmt *)

type vtyp =
  | Texp
  | Tarr
  | Tobj
  | Tint
  | Tstr

module type Object = sig
  type k
  type v
  module M : Map.S with type key = k
  val v : v M.t
  val typ : vtyp option
end

type value =
  | Vint of int
  | Varray of value array
  | Vobj of value t_object
  | Vlp_exp of Expr.t
  | Vstring of string

(* hack to avoid recursive modules *)
and 'a t_object = (module Object with type v = 'a and type k = value)

let obj_bindings (type x) (o:x t_object) =
  let module O = (val o : Object with type v = x and type k = value) in
  O.M.bindings O.v

module Value = struct
  type t = value

  let rec compare v1 v2 = match v1,v2 with
    | Vint i1, Vint i2 -> Stdlib.compare i1 i2
    | Vstring s1, Vstring s2 -> Stdlib.compare s1 s2
    | Vobj o1, Vobj o2 -> compare_obj o1 o2
    | Vlp_exp e1, Vlp_exp e2 -> Expr.compare e1 e2
    | Varray a1, Varray a2 -> compare_array a1 a2

    | Vint _, _ -> -1
    | _, Vint _ -> compare v2 v1
    | Varray _, _ -> -1
    | _, Varray _ -> compare v2 v1
    | Vobj _, _ -> -1
    | _, Vobj _ -> compare v2 v1
    | Vlp_exp _, _ -> -1
    | _, Vlp_exp _ -> compare v2 v1

  and compare_array a1 a2 =
    let len1 = Array.length a1 in
    let len2 = Array.length a2 in
    if len1 < len2
    then -1
    else if len1 > len2
    then 1
    else
      let rec aux i =
        if i = len1
        then 0
        else
          let cmp = compare a1.(i) a2.(i) in
          if cmp = 0
          then aux (i+1)
          else cmp
      in
      aux 0

  and compare_obj o1 o2 =
    (* slow, but simple... *)
    let l1 = obj_bindings o1 in
    let l2 = obj_bindings o2 in
    let rec aux l1 l2 = match l1, l2 with
      | [], [] -> 0
      | [], _ -> -1
      | _, [] -> 1
      | (k1,v1) :: q1, (k2,v2) :: q2 ->
        let ck = compare k1 k2 in
        if ck = 0
        then let cv = compare v1 v2 in
          if cv = 0
          then aux q1 q2
          else cv
        else ck
    in
    aux l1 l2

  let rec print_value ppf = function
    | Vint i -> Format.pp_print_int ppf i
    | Varray a ->
      let len = Array.length a in
      if len > 0
      then
        begin Format.fprintf ppf "[@[<1>@ ";
          for i = 0 to Array.length a - 2 do
            Format.fprintf ppf "%a,@ " print_value a.(i);
          done;
          Format.fprintf ppf "%a@ " print_value a.(len-1);
          Format.fprintf ppf "]@]@ "
        end
      else Format.fprintf ppf "[]";
    | Vstring s -> Format.pp_print_string ppf s;
    | _ -> failwith "TODO print value"

end

module ValMap = Map.Make(Value)

module Object : sig
  type 'a t = 'a t_object
  val mem : 'a t -> Value.t -> bool
  val compare : Value.t t -> Value.t t -> int
  val add : Value.t -> 'a -> 'a t -> Loc.t -> 'a t
  val empty : 'a t
  val find : Value.t -> 'a t -> Loc.t -> 'a
  val bindings : 'a t -> (Value.t * 'a) list
  val map : ('a -> 'b) -> 'a t -> 'b t
  val merge : (Value.t -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
end = struct

  type 'a t = 'a t_object

  let mem (type x) obj key =
  let module O = (val obj : Object with type v = x and type k = value) in
    O.M.mem key O.v

  let compare = Value.compare_obj

  let add (type x) key value obj loc =
    (* TODO: check type ? *)
    if mem obj key
    then error_gen loc
        (fun ppf -> Format.fprintf ppf "Unbound field %a" Value.print_value key)
    else
      let module O = (val obj : Object with type v = x and type k = value) in
      let module R = struct
        include O
        let v = O.M.add key value O.v
      end in
      (module R : Object with type v = x and type k = value)

  let empty (type x) =
    let module R = struct
      type v = x
      type k = value
      module M = ValMap
      let typ = None
      let v = ValMap.empty
    end in
    (module R : Object with type v = x and type k = value)

  let find (type x) key obj loc =
    let module O = (val obj : Object with type v = x and type k = value) in
    try O.M.find key O.v
    with Not_found ->
      raise (Error (Unbound_field
                      (fun ppf -> Value.print_value ppf key),loc))

  let bindings (type x) obj =
    let module O = (val obj : Object with type v = x and type k = value) in
    O.M.bindings O.v

  let map (type x) (type y) f (obj: x t) =
    let module O = (val obj : Object with type v = x and type k = value) in
    let module R = struct
      type v = y
      type k = value
      module M = ValMap
      let typ = None
      let v = O.M.fold (fun k v map -> M.add k (f v) map) O.v M.empty
    end in
    (module R : Object with type v = y and type k = value)


  let merge (type x) f (obj1:'a t) (obj2:'b t) =
    let l1 = obj_bindings obj1 in
    let l2 = obj_bindings obj2 in
    let add (id:Value.t) v1 v2 acc =
      match f id v1 v2 with
      | None -> acc
      | Some r -> ValMap.add id r acc
    in
    let rec aux l1 l2 acc = match l1, l2 with
      | [], [] -> acc
      | [], (id,t)::q -> aux [] q (add id None (Some t) acc)
      | (id,t)::q, [] -> aux q [] (add id (Some t) None acc)
      | (id1,t1)::q1, (id2,t2)::q2 ->
        let c = Value.compare id1 id2 in
        if c = 0
        then aux q1 q2 (add id1 (Some t1) (Some t2) acc)
        else if c < 0
        then aux q1 l2 (add id1 (Some t1) None acc)
        else aux l1 q2 (add id2 None (Some t2) acc)
    in
    let module R = struct
      type v = x
      type k = value
      module M = ValMap
      let typ = None
      let v = aux l1 l2 ValMap.empty
    end in
    (module R : Object with type v = x and type k = value)

end

include Value

let print_val v ppf = print_value ppf v

let vtyp = function
  | Vint _ -> Tint
  | Varray _ -> Tarr
  | Vobj _ -> Tobj
  | Vlp_exp _ -> Texp
  | Vstring _ -> Tstr


module Ident = struct
  type t = string
  let compare = String.compare
  let create name = name
end

module Env = struct

  module IdMap = Map.Make(Ident)

  type t =
    { values : value IdMap.t;
      types : typ IdMap.t; }

  let empty =
    { values = IdMap.empty;
      types = IdMap.empty; }

  let find_val (env:t) v loc =
    try
      IdMap.find v env.values
    with Not_found ->
      raise (Error (Unbound_variable v,loc))

  let find_typ (env:t) v loc =
    try
      IdMap.find v env.types
    with Not_found ->
      raise (Error (Unbound_variable v,loc))

  let add_table m id v loc =
    if IdMap.mem id m
    then raise (Error (Mutliple_declaration id,loc))
    else IdMap.add id v m

  let add env id v t loc =
    { values = add_table env.values id v loc;
      types = add_table env.types id t loc }

  let add_typ env id t loc =
    { env with types = add_table env.types id t loc }

  let bind_var env id v loc =
    { env with values = add_table env.values id v loc }

  let int_typ env =
    IdMap.fold (fun id t l ->
      match t with
      | Int | Bool -> id::l
      | _ -> l)
      env.types []

  let bool_typ env =
    IdMap.fold (fun id t l ->
      match t with
      | Bool -> id::l
      | _ -> l)
      env.types []

  let get_val env id =
    try
      Some (IdMap.find id env.values)
    with Not_found ->
      None

  let replace_var env id v =
    { env with values = IdMap.add id v env.values }

end

type bool_result =
  | Constraints of Lp.constr list
  | Bool of bool

type btyp =
  | Tconstr
  | Tbool

let btyp = function
  | Constraints _ -> Tconstr
  | Bool _ -> Tbool

(* Error reporting *)

open Format

let report_error ppf = function
  | Unbound_variable v ->
    fprintf ppf "Unbound variable %s" v
  | Mutliple_declaration v ->
    fprintf ppf "Variable %s bound multiple times" v
  | Mutliple_declaration_field v ->
    fprintf ppf "Object field %s bound multiple times" v
  | General f ->
    f ppf
  | Unbound_field f ->
    fprintf ppf "No field %a in object" (fun ppf () -> f ppf) ()
