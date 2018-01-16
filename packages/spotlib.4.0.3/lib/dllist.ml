(* doubly linked list *)

module type S = sig

  type 'a t
    (** The type of the dllist *)

  type 'a node
    (** The type of the dllist node *)

  val create : unit -> 'a t
    (** Create an empty dllist *)

  val length : 'a t -> int
    (** O(1). The length of the dllist *)

  val is_empty : 'a t -> bool
    
  val list : 'a node -> 'a t option
    (** [list node] returns [Some t] if [node] is an element of [t].
        If [node] is removed, it returns [None]. *)

  val is_removed : 'a node -> bool

  val value : 'a node -> 'a
    (** Get the value from the node *)    

  val add : 'a t -> 'a -> 'a node
    (** O(1). [add t v] adds [v] to dllist [t] and returns the newly created
        node for [v]. The node is used to remove the value from [t] in constant
        time.
    *)

  val remove : 'a node -> (unit, [> `Already_removed]) result
    (** O(1). [remove node] removes [node] from the dllist it belongs to.
        Successful removal returns [`Ok]. If the node is already removed,
        [remove node] returns [`Already_removed]. *)

  val hd : 'a t -> 'a node option
    (** [hd t] returns the first node of the dllist [t]. *)

  val tl : 'a t -> 'a node option option
    (** [tl t] returns the second node of the dllist [t]. 

        None : t is null
        Some None : t is a singleton
        Some (Some n) : n is the second
    *)

  val hd_tl : 'a t -> ('a node * 'a node option) option

  val iter : ('a node -> unit) -> 'a t -> unit
    (** Iteration over the nodes of a dllist from the top to the bottom *)

  val fold_left : ('a -> 'b node -> 'a) -> 'a -> 'b t -> 'a
    (** Folding the nodes of a dllist from the top to the bottom *)

  val fold_right : ('b node -> 'a -> 'a) -> 'b t -> 'a -> 'a
    (** Folding the nodes of a dllist from the bottom to top *)

  val scan_left : ('a -> 'b node -> [< `Continue of 'a | `Stop of 'a ]) ->
    'a -> 'b t -> 'a
    (** [fold] with stop *)

  val scan_left_nodes : ('a -> 'b node -> [< `Continue of 'a | `Stop of 'a ]) ->
    'a -> 'b node -> 'a
    (** [scan] but starts with a node *)

  (** list <=> dllist conversion functions *)    
  val to_nodes : 'a t -> 'a node list
  val to_list : 'a t -> 'a list
  val of_list : 'a list -> 'a t

  val invariant : 'a t -> unit
    (** Invariant checks *)

end

module Z : S = struct
  type 'a node = {
    mutable prev : 'a node option;
    mutable next : 'a node option;
    mutable parent : 'a t option;
    value : 'a;
  }
  
  and 'a t = {
    mutable top : 'a node option;
    mutable bottom : 'a node option;
    mutable length : int;
  }
  
  let create () = 
    { top = None;
      bottom = None;
      length = 0 }
  ;;
  
  let length t = t.length
  let is_empty t = t.length = 0
  
  let list node = node.parent
  let is_removed node = node.parent = None
  let value node = node.value
  
  let add t v =
    let node = { prev = t.bottom;
                 next = None;
                 parent = Some t;
                 value = v }
    in
    begin match t.bottom with
    | None -> t.top <- Some node
    | Some bottom -> bottom.next <- Some node
    end;
    t.bottom <- Some node;
    t.length <- t.length + 1;
    node
  ;;
  
  let remove node =
    match node.parent with
    | None -> Error `Already_removed
    | Some t ->
        let top =
          match node.prev with
          | None -> node.next
          | Some prev -> 
  	    prev.next <- node.next;
  	    t.top 
        in
        let bottom = 
          match node.next with
          | None -> node.prev
          | Some next -> 
  	    next.prev <- node.prev;
  	    t.bottom
        in
        node.parent <- None;
        node.prev <- None;
        node.next <- None;
        t.length <- t.length - 1; 
        t.top <- top;
        t.bottom <- bottom;
        Ok ()
  ;;
  
  let hd t = 
    match t.top with
    | Some node -> Some node
    | None -> None
  ;;
  
  let tl t = 
    match t.top with
    | Some node -> Some node.next
    | None -> None
  ;;

  let hd_tl t = match t.top with
    | None -> None
    | Some node -> Some (node, node.next)

  let iter f t =
    let rec iter = function
      | None -> ()
      | Some node -> f node; iter node.next
    in
    iter t.top
  ;;
  
  let fold_left f init t =
    let rec fold acc = function
      | None -> acc
      | Some node -> fold (f acc node) node.next
    in
    fold init t.top
  ;;
  
  let gen_scan f init nopt =
    let rec scan acc = function
      | None -> acc
      | Some node -> 
          match f acc node with
          | `Stop acc' -> acc'
          | `Continue acc' -> scan acc' node.next
    in
    scan init nopt
  ;;
  
  let scan_left_nodes f init n = gen_scan f init (Some n)
    
  let scan_left f init t = gen_scan f init t.top
  
  let fold_right f t init =
    let rec fold v acc = match v with
      | None -> acc
      | Some node -> fold node.prev (f node acc) 
    in
    fold t.bottom init
  ;;
  
  let to_nodes t = fold_right (fun x acc -> x :: acc) t []
  let to_list t = fold_right (fun x acc -> x.value :: acc) t []
  
  let of_list l =
    let t = create () in
    List.iter (fun x -> ignore (add t x)) l; 
    t
  ;;
  
  (* invariants *)
      
  let invariant_node node =
    match node.parent with
    | None -> 
        assert (node.prev = None);
        assert (node.next = None);
    | Some t ->
        let self_prev =
          match node.prev with
          | None -> t.top
          | Some prev -> prev.next
        in
        let self_next =
          match node.next with
          | None -> t.bottom
          | Some next -> next.prev
        in
        let check_self = function
          | None -> assert false
          | Some self -> assert (self == node)
        in
        check_self self_prev;
        check_self self_next
  ;;
  
  let invariant t =
    let counted_length =
      fold_left (fun acc node ->
        begin match node.parent with
        | None -> assert false
        | Some parent -> assert (t == parent)
        end;
        invariant_node node;
        acc + 1) 0 t 
    in
    if t.length <> counted_length then begin
      Printf.eprintf "length=%d counted=%d\n"
        t.length counted_length;
      assert false
    end
  ;;  
end

module Test(Z:S) = struct (* check the types first *)

  open Z

  (* very simple test *)
  let () =
    let t = create () in
    let node = add t 1 in
    begin match hd t with
    | None -> assert false
    | Some node -> assert (value node = 1)
    end;
    assert (to_list t = [1]);
    assert (remove node = Ok ());
    assert (is_empty t);
    prerr_endline "very simple: passed";
  ;;

  (* to_list . of_list = ident *)
  let () =
    let ints = 
      let rec ints acc = function
        | 0 -> acc
        | n -> ints (n::acc) (n-1) 
      in
      ints [] 10000
    in
    let t = of_list ints in
    invariant t;
    assert (to_list (of_list ints) = ints);
    prerr_endline "to_list . of_list = id: passed"
  ;;

  (* misc api test *)
  let () = 
    let t = create () in
    assert (is_empty t);

    let ints = [1;2;3;4;5;6;7;8;9;10] in 
    let t = of_list ints in

    let s = ref [] in
    iter (fun node -> s := value node :: !s) t;
    assert (List.rev ints = !s);

    assert (55 = fold_left (fun acc node -> acc + value node) 0 t);
    assert (ints = fold_right (fun node acc -> value node :: acc) t []);
    prerr_endline "misc api test: passed";
  ;;
  
  (* random add/removal test *)
  let () =
    let t = create () in
    (* get a random element of a list, one path *)
    let random_in_list = function
      | [] -> None
      | x::xs ->
          let rec random_in_list len cand = function
            | [] -> cand
            | x::xs ->
                (* cand survives : len/(len+1) *)
                (* x overrides : 1/(len+1) *)
                let cand = 
                  if Random.int (len+1) = 0 then x
                  else cand
                in
                random_in_list (len+1) cand xs
          in
          Some (random_in_list 1 x xs)
    in
  
    let rec loop added rev_current = function
      | 10000 -> rev_current
      | n ->
          invariant t;
          if Random.int 3 = 0 then begin
            let rev_current =
              match random_in_list added with
              | None -> rev_current
              | Some node ->
                  let removed = is_removed node in
                  match removed, remove node with
                  | true, Error `Already_removed -> rev_current
                  | false, Ok _ ->
                      List.filter (fun x -> x != node) rev_current 
                  | _ -> assert false
            in
            loop added rev_current n
          end else 
            let node = add t n in
            loop (node :: added) (node :: rev_current) (n+1)
    in
    let rev_current = loop [] [] 0 in 
    invariant t;
    assert (to_list t = List.rev_map value rev_current);
    (* remove all the elements remaining *)
    let rec f rev_current =
      match random_in_list rev_current with
      | None -> assert (is_empty t)
      | Some node ->
          assert (remove node = Ok ());
          invariant t;
          f (List.filter (fun x -> x != node) rev_current)
    in
    f rev_current;
    prerr_endline "big random add/remove test: passed";
  ;;

end    

[%%TEST
  let () = 
    let module TZ = Test(Z) in
    true
]

include Z
