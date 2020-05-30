(* weak hash table *)

(* bucket array *)
module Bucket : sig
  type ('a, 'b) t
  val create : int -> ('a, 'b) t
  val add : ('a, 'b) t -> 'a -> 'b -> unit
  val find : ('a, 'b) t -> ('a -> 'b -> bool) -> ('a * 'b) option
  (** [find t f]: the first [(k,v)] pair [f k v = true], if exists, is returned as [Some (k,v)].
      If none found, returns None *)

  val remove : ('a, 'b) t -> ('a -> 'b -> bool) -> ('a * 'b) option
  (** [remove t f]: the first [(k,v)] pair [f k v = true], if exists, is removed from [t] and returns the [Some (k,v)].
      If none found, returns None. *) 

  val removeq : ('a, 'b) t -> 'a -> 'b option
  (** [removeq t k] removes the binding of the pointer equal [k] from [t] and returns its value if exists. 
      Otherwise it returns [None]. *)
    
  val length : ('a, 'b) t -> int 
  (** return the number of full elements *)
    
end = struct
  type ('a, 'b) t = { 
    mutable keys : 'a Weak.t;
    mutable values : 'b option array; (* CR: No point of having option. We can safely use Obj *)
    mutable size : int;               (* size of keys *)
    init_size : int;                  (* initial size *)
    mutable cur : int;                (* keys must be all empty from cur to size-1 *)
    mutable nelems : int;             (* elements in the bucket *)
  }

  let create size = { 
    keys = Weak.create size; (* CR size > 0 and max_array_length *) 
    values = Array.make size None;
    size = size;
    init_size = size;
    cur = 0;
    nelems = 0;
  }

  (* If [t == t'], compaction in place. Otherwise, compaction by copy.
     At compaction by copy, there is no check of destination size.
  *)
  let rec compact t t' to_ from = 
    if from = t.size then begin (* finished. clear from to_ to the end *)
      for i = to_ to t'.size - 1 do
        Weak.set t'.keys i None;
        Array.unsafe_set t'.values i None;
      done;
      to_ (* returns # of filled elements *)
    end else 
      match Weak.get t.keys from with
      | None -> compact t t' to_ (from+1)
      | somev ->
          Weak.set t'.keys to_ somev;
          Array.unsafe_set t'.values to_ (Array.unsafe_get t.values from);
          compact t t' (to_+1) (from+1)

  let compact t t' = compact t t' 0 0

  let enlarge t =
    let newsize = t.size * 2 in (* CR: Sys.max_array_length *)
    let keys = t.keys in
    let keys' = Weak.create newsize in
    let values = t.values in
    let values' = Array.make newsize None in
    Weak.blit keys 0 keys' 0 t.size;
    Array.blit values 0 values' 0 t.size;
    t.keys <- keys';
    t.values <- values';
    t.size <- newsize;
    t.cur <- t.size

  let shrink t =
    let newsize = max (t.size / 2) t.init_size in
    if newsize < t.nelems then begin
      let t' = create newsize in (* t' is a different bucket but contents will be copied to t *)
      (* compaction by copy *)
      ignore (compact t t');
      t.keys <- t'.keys;
      t.values <- t'.values;
      t.size <- t'.size;
      t.cur <- t'.cur;
    end
      
  let find_gen t f =
    let rec find t f i = 
      if i = t.size then None
      else
        match Weak.get t.keys i with
        | None -> find t f (i+1)
        | Some k ->
            match Array.unsafe_get t.values i with
            | None -> assert false
            | Some v -> if f k v then Some (k,v,i) else find t f (i+1)
    in
    find t f 0

  let find t f = 
    match find_gen t f with
    | None -> None
    | Some (k,v,_) -> Some (k,v)

  let remove t f =
    match find_gen t f with
    | None -> None
    | Some (k,v,i) -> 
        Weak.set t.keys i None;
        Array.unsafe_set t.values i None;
        t.nelems <- t.nelems - 1;
        shrink t;
        Some (k,v)

  let compact_and_may_enlarge t = if compact t t = t.size then enlarge t

  let removeq t k = 
    match remove t (fun k' _v -> k == k') with
    | Some (_, v) -> Some v
    | None -> None

  let removeq_gc t k = assert (removeq t k <> None)

  let rec add t k v = 
    if t.cur < t.size then begin 
      Gc.finalise (removeq_gc t) k;
      Weak.set t.keys t.cur (Some k);
      Array.unsafe_set t.values t.cur (Some v);
      t.cur <- t.cur + 1;
      t.nelems <- t.nelems + 1;
    end else begin
      (* compact and may enlarge it then try again *)
      compact_and_may_enlarge t;
      add t k v
    end

  let length t = t.nelems
end

module Make(K : Hashtbl.HashedType) : sig
  type 'a t
  val create : int -> 'a t
  val add : 'a t -> K.t -> 'a -> unit
  val find : 'a t -> K.t -> (K.t * 'a) option
  val findq : 'a t -> K.t -> 'a option
  val mem : 'a t -> K.t -> bool
  val memq : 'a t -> K.t -> bool
  val remove : 'a t -> K.t -> (K.t * 'a) option
  val removeq : 'a t -> K.t -> 'a option
  val length : 'a t -> int
end = struct
  type 'a t = (K.t, 'a) Bucket.t array

  let create size = Array.init size (fun _ -> Bucket.create 10)

  let get_bucket t k =
    let pos = (K.hash k) mod (Array.length t) in
    Array.unsafe_get t pos
    
  let add t k v =
    let bucket = get_bucket t k in
    Bucket.add bucket k v

  let find t k =
    let bucket = get_bucket t k in
    Bucket.find bucket (fun k' _ -> K.equal k k')

  let findq t k =
    let bucket = get_bucket t k in
    match Bucket.find bucket (fun k' _ -> k == k') with
    | Some (_,v) -> Some v
    | None -> None 

  let mem t k = find t k <> None
  let memq t k = findq t k <> None

  let remove t k =
    let bucket = get_bucket t k in
    Bucket.remove bucket (fun k' _ -> K.equal k k')
    
  let removeq t k =
    let bucket = get_bucket t k in
    Bucket.removeq bucket k

  (* CR jfuruse: O(n) where n is the size of buckets *)      
  let length t = Array.fold_left (fun st b -> st + Bucket.length b) 0 t
    
end

module O = Make(struct
  type t = Obj.t
  let hash = Hashtbl.hash
  let equal = (=)
end)

module Poly : sig
  type ('a, 'b) t
  val create : int -> ('a, 'b) t
  val add : ('a, 'b) t-> 'a -> 'b -> unit
  val find : ('a, 'b) t-> 'a -> ('a * 'b) option
  val findq : ('a, 'b) t-> 'a -> 'b option
  val remove : ('a, 'b) t-> 'a -> ('a * 'b) option
  val removeq : ('a, 'b) t-> 'a -> 'b option
  val length : ('a, 'b) t -> int
end = struct
  type ('a, 'b) t = 'b O.t
  let coerce_opt = function
    | None -> None
    | Some (k,v) -> Some (Obj.obj k, v)
  let create = O.create
  let add t k v = O.add t (Obj.repr k) v
  let find t k = coerce_opt (O.find t (Obj.repr k))
  let findq t k = O.findq t (Obj.repr k)
  let remove t k = coerce_opt (O.remove t (Obj.repr k))
  let removeq t k = O.removeq t (Obj.repr k)
  let length = O.length
end

include Poly
