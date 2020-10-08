(* This decoration is required because hashconsing for standart types is
 * not yet included in GT module
 *
 * Actual example is below.
 *)
module GT = struct
  include GT

  module H :
  sig
    type t
    (* Call [hc tbl a] return (possibly) updated hash table with
     * possible old value if [a] was already seen.
     *)
    val hc : t -> 'a -> t * 'a
    val create : unit -> t
  end =
  struct
    module H = Hashtbl.Make (struct
        type t = Obj.t
        let hash = Hashtbl.hash
        let equal new_ old =
          (* Printf.printf "%s %d\n%!" __FILE__ __LINE__; *)
          if Hashtbl.hash new_ <> Hashtbl.hash old
          then false
          else if Obj.(tag @@ repr new_) <> Obj.(tag @@ repr old)
          then false
          else if Obj.(size @@ repr new_) <> Obj.(size @@ repr old)
          then false
          else
            List.fold_left (fun acc n ->
                (Obj.field (Obj.repr new_) n == Obj.field (Obj.repr old) n) && acc
              ) true (List.init Obj.(size @@ repr old) (fun n -> n))

      end)
    type t = Obj.t H.t

    let create () = H.create 37
    let hc h x =
      (* Printf.printf "%s %d\n%!" __FILE__ __LINE__; *)
      if Obj.(is_int @@ repr x) then (h,x)
      else
        let o = Obj.repr x in
        try
          let old = Obj.magic @@ H.find h o in
          print_endline "use old value";
          (h,old)
        with Not_found ->
          H.add h o o;
          print_endline "use new value";
          h, x
  end

  type h = {hc : 'a . H.t -> 'a -> H.t * 'a}

  let hf = {hc = H.hc}

  let int =
    { GT.gcata = int.GT.gcata
    ; fix = (fun c -> transform_gc gcata_int c)
    ; plugins = object
        method hash h n = (h,n)
      end
    }

  let hash c = c.plugins#hash
end

(* Interesting part goes here *)

type expr = Const of GT.int | Binop of expr * expr
[@@deriving gt ~options: { hash } ]

(* reuses old (Const 5) when accessing another (Const 5) *)
let (_,_) =
  let h = GT.H.create () in
  (* [GT.hash typ] takes a value and return its copy where all equal subtrees
   * have the same location in memory
   *)
  GT.hash (expr) h (Binop (Const 5, Const 5))
