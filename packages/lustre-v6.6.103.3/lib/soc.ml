(* Time-stamp: <modified the 21/03/2018 (at 17:17) by Erwan Jahier> *)

(** Synchronous Object Component *)

(* Just a string because :
   - it's more ocamldebug-friendly  
   - Name clashing issues ougth to have been fixed before
 *)
type ident = string

type var = ident * Data.t

(* Variable denotation *)
type var_expr =
  | Var   of var
  | Const of var (* useful? *)
  | Field of var_expr * ident * Data.t
  | Index of var_expr * int * Data.t
  | Slice of var_expr * int * int * int * int * Data.t (* first, last, step, width *)

type key_opt =
  | Nomore
  | Slic of int * int * int (* for slices *)
  | MemInit of var_expr (* for fby *)
  | Curr of Lv6Id.long  (* clock constructor for current *)

type key = 
    ident *
    Data.t list *  (* I/O type list *)
    key_opt

type instance = ident * key


let (data_type_of_var_expr : var_expr -> Data.t) =
  function
  | Var(_,vt)
  | Const(_,vt)
  | Field(_, _,vt)
  | Index(_,_,vt) 
  | Slice(_,_,_,_,_,vt) -> vt

type atomic_operation =
  | Assign (* Wire *)
  | Method    of instance * ident (* node step call ; the ident is the step name *)
  | Procedure of key (* memoryless method made explicit (a good idea?) *)

(* Guarded Atomic Operation *)
type gao =
  | Case of ident (* enum var *) * (ident (* enum value *) * gao list) list * Lxm.t
  | Call of var_expr list * atomic_operation * var_expr list * Lxm.t
         (* outputs (lhs) * op               * inputs (rhs) *)

type step_impl =
  | Predef 
  | Gaol of var list * gao list  (* local vars + body *)
  | Iterator of string * key * int (* iterator, iterated soc key, size *)
  | Boolred of int  * int * int 
  | Condact of key * var_expr list (* condact-ed node, default constants *)
  | Extern

type step_method = {
  name    : ident;
  lxm     : Lxm.t;
(* XXX c'est laid ces histoires d'index. y'a qu'a recopier les
   variables nécessaires et puis c'est marre !!! *)
  idx_ins  : int list; (* input  index in the profile *)
  idx_outs : int list; (* output index in the profile *)
  impl    : step_impl;
(*   impl    : (var list * gao list) option; (* local vars + body ; None for predef op *) *)

}

type precedence = ident * ident list   
(* Partial order over step members. Maps a step method name with
   the list of step methods that should be called _before_.
   
   nb : steps in step are already ordered ; this partial order
   can be useful to find another (better) total order w.r.t. test
   opening.
*)

type memory = 
  | No_mem
  | Mem of Data.t 
  | Mem_hidden (* for extern nodes *)

type t = { 
  key      : key;
  profile  : var list * var list;
  clock_profile : (var * (var * Lv6Id.long)) list;
  (* associate to (profile) var a clock, if not on base *)
  step     : step_method list; (* the order in the list is a valid w.r.t. 
                                  the partial order defined in precedences *)
  precedences : precedence list; (* partial order over step methods *)
  instances : instance list;
  memory : memory; 
  (* Do this soc have a memory (pre, fby) + its type *)
  assertions: (Lxm.t * var) list;
}

(* SocKeyMap ? *)
module SocMap = Map.Make(
  struct
    type t = key
    let compare = compare
  end
)

(* XXX Faire une table a 2 niveaux d'index: 
  - le nom
   - le type 

 *)
type tbl = t SocMap.t
(* cf SocUtils  *)
             
    
let cpt = ref 0
let (make: key -> instance) = 
  fun sk -> 
    let (id,_,_) = sk in
    let instance = Printf.sprintf "%s%03d" id !cpt in
    incr cpt;
    instance,sk

