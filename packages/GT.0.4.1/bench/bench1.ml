module Lists = struct
  let name = "lists"

  let create n =
    let rec helper acc m =
      if m<n then helper (()::acc) (m+1) else acc
    in
    helper [] 0

  module A = struct
    let rec map1list xs =
      match xs with
      | [] -> []
      | x::xs -> x::(map1list xs)

    let map () xs = map1list xs

    let fold i xs =
      let f () _ = () in
      let rec helper acc = function
      | [] -> i
      | x::xs -> helper (f acc x) xs
      in
      helper i xs
  end

  module B = struct
    let o = object(self)
      method c_Cons () x xs = x :: (self#gcata () xs)
      method c_Nil () = []
      method gcata () = function
      | x::xs -> self#c_Cons () x xs
      | [] -> self#c_Nil ()
    end

    let map () xs = o#gcata () xs

    let folder f = object(self)
      method c_Cons i x xs = self#gcata (f i x) xs
      method c_Nil i = i
      method gcata i = function
      | x::xs -> self#c_Cons i x xs
      | [] -> self#c_Nil i
    end

    let fold () xs =
      (folder (fun () _ -> ()))#gcata () xs
  end

  module C = struct
    let o fself = object
      method c_Cons () x xs = x :: (fself () xs)
      method c_Nil () = []
    end

    let gcata self () = function
      | x::xs -> self#c_Cons () x xs
      | [] -> self#c_Nil ()

    let map () xs =
      let rec obj = lazy (o fself)
      and fself () x = gcata (Lazy.force obj) () x
      in
      fself () xs

    let folder f fself = object
      method c_Cons i x xs = fself (f i x) xs
      method c_Nil i  = i
    end

    let fold () xs =
      let f () _ = ()  in
      let rec obj = lazy (folder f fself)
      and fself () x = gcata (Lazy.force obj) () x
      in
      fself () xs

  end
end


module Trees = struct
  type tree = Leaf | Node of tree * tree
  let name = "trees"


  let create n =
    let rec helper acc m =
      if m<n then helper (Node (acc, acc)) (m+1) else acc
    in
    helper Leaf 0

  module A = struct
    let rec map1list xs =
      match xs with
      | Leaf -> Leaf
      | Node (l,r) -> Node (map1list l, map1list r)

    let map () xs = map1list xs

    let f () _ = ()

    let rec fold () xs =
      match xs with
      | Leaf -> ()
      | Node (l,r) -> fold (fold () l) r

  end

  module B = struct
    let o = object(self)
      method c_Node () l r = Node (self#gcata () l, self#gcata () r)
      method c_Leaf () = Leaf
      method gcata () = function
      | Node (l,r) -> self#c_Node () l r
      | Leaf -> self#c_Leaf ()
    end

    let map () xs = o#gcata () xs

    let folder = object(self)
      method c_Node i l r =  self#gcata (self#gcata i l) r
      method c_Leaf () = ()
      method gcata () = function
      | Node (l,r) -> self#c_Node () l r
      | Leaf -> self#c_Leaf ()
    end

    let fold () t = folder#gcata () t

  end

  module C = struct
    let o fself = object
      method c_Node () l r = Node (fself () l, fself () r)
      method c_Leaf () = Leaf
    end

    let gcata self () = function
      | Node (l,r) -> self#c_Node () l r
      | Leaf -> self#c_Leaf ()

    let map () xs =
      let rec obj = lazy (o fself)
      and fself () x = gcata (Lazy.force obj) () x
      in
      fself () xs

    let folder fself = object
      method c_Node i l r =  fself (fself i l) r
      method c_Leaf i = i
    end

    let fold () xs =
      let rec obj = lazy (folder fself)
      and fself () x = gcata (Lazy.force obj) () x
      in
      fself () xs
  end
end



module ManyConstrs = struct
  let name = "many_constructors"
  type tree = Leaf
    | Node1 of tree * tree
    | Node2 of tree * tree
    | Node3 of tree * tree
    | Node4 of tree * tree
    | Node5 of tree * tree
    (* | Node6 of tree * tree
    | Node7 of tree * tree
    | Node8 of tree * tree
    | Node9 of tree * tree *)
    | Node  of tree * tree

  let create n =
    let rec helper acc m =
      if m<n then helper (Node (acc, acc)) (m+1) else acc
    in
    helper Leaf 0

  module A = struct
    let rec map1list xs =
      match xs with
      | Leaf -> Leaf
      | Node1 (_,_) -> failwith "should not happen"
      | Node2 (_,_) -> failwith "should not happen"
      | Node3 (_,_) -> failwith "should not happen"
      | Node4 (_,_) -> failwith "should not happen"
      | Node5 (_,_) -> failwith "should not happen"
      (* | Node6 (_,_) -> failwith "should not happen"
      | Node7 (_,_) -> failwith "should not happen"
      | Node8 (_,_) -> failwith "should not happen"
      | Node9 (_,_) -> failwith "should not happen" *)
      | Node (l,r) -> Node (map1list l, map1list r)

    let map () xs = map1list xs

    let rec fold i = function
    | Leaf -> ()
    | Node1 (_,_) -> failwith "should not happen"
    | Node2 (_,_) -> failwith "should not happen"
    | Node3 (_,_) -> failwith "should not happen"
    | Node4 (_,_) -> failwith "should not happen"
    | Node5 (_,_) -> failwith "should not happen"
    (* | Node6 (_,_) -> failwith "should not happen"
    | Node7 (_,_) -> failwith "should not happen"
    | Node8 (_,_) -> failwith "should not happen"
    | Node9 (_,_) -> failwith "should not happen" *)
    | Node (l,r) -> fold (fold i l) r
  end

  module B = struct
    let o = object(self)
      method c_Node () l r = Node (self#gcata () l, self#gcata () r)
      method c_Leaf () = Leaf
      method gcata () = function
      | Node (l,r) -> self#c_Node () l r
      | Node1 (_,_) -> failwith "should not happen"
      | Node2 (_,_) -> failwith "should not happen"
      | Node3 (_,_) -> failwith "should not happen"
      | Node4 (_,_) -> failwith "should not happen"
      | Node5 (_,_) -> failwith "should not happen"
      (* | Node6 (_,_) -> failwith "should not happen"
      | Node7 (_,_) -> failwith "should not happen"
      | Node8 (_,_) -> failwith "should not happen"
      | Node9 (_,_) -> failwith "should not happen" *)
      | Leaf -> self#c_Leaf ()
    end

    let map () xs = o#gcata () xs

    let folder = object(self)
      method c_Node i l r = self#gcata (self#gcata i l) r
      method c_Leaf i = i

      method gcata () = function
      | Node (l,r) -> self#c_Node () l r
      | Node1 (_,_) -> failwith "should not happen"
      | Node2 (_,_) -> failwith "should not happen"
      | Node3 (_,_) -> failwith "should not happen"
      | Node4 (_,_) -> failwith "should not happen"
      | Node5 (_,_) -> failwith "should not happen"
      (* | Node6 (_,_) -> failwith "should not happen"
      | Node7 (_,_) -> failwith "should not happen"
      | Node8 (_,_) -> failwith "should not happen"
      | Node9 (_,_) -> failwith "should not happen" *)
      | Leaf -> self#c_Leaf ()
    end
    let fold () xs = folder#gcata () xs
  end

  module C = struct
    let o fself = object
      method c_Node () l r = Node (fself () l, fself () r)
      method c_Leaf () = Leaf
    end

    let gcata self () = function
      | Node (l,r) -> self#c_Node () l r
      | Node1 (_,_) -> failwith "should not happen"
      | Node2 (_,_) -> failwith "should not happen"
      | Node3 (_,_) -> failwith "should not happen"
      | Node4 (_,_) -> failwith "should not happen"
      | Node5 (_,_) -> failwith "should not happen"
      (* | Node6 (_,_) -> failwith "should not happen"
      | Node7 (_,_) -> failwith "should not happen"
      | Node8 (_,_) -> failwith "should not happen"
      | Node9 (_,_) -> failwith "should not happen" *)
      | Leaf -> self#c_Leaf ()

    let map () xs =
      let rec obj = lazy (o fself)
      and fself () x = gcata (Lazy.force obj) () x
      in
      fself () xs

    let folder fself = object
      method c_Node i l r = fself (fself i l) r
      method c_Leaf i = i
    end

    let fold () t =
      let rec obj = lazy (folder fself)
      and fself () x = gcata (Lazy.force obj) () x in
      fself () t
  end
end

open Benchmark

let timeout = 1
let repeat = 1

let __ () =
  let module M = Lists in
  [300; 500; 700; 900; 1000 ] |> List.iter (fun n ->
    let xs = M.create n in
    let wrap f () =
      (* Gc.major ();
      Gc.minor ();
      Gc.compact (); *)
      let _ = f () xs  in
      ()
    in
    let res =
      throughputN ~repeat timeout
        [ (Printf.sprintf "%s_D_%d" M.name n, wrap M.A.fold, ())
        ; (Printf.sprintf "%s_V" M.name, wrap M.B.fold, ())
        ; (Printf.sprintf "%s_G" M.name, wrap M.C.fold, ())
        ]
    in
    print_newline ();
    tabulate res
  )



(* folding lists *)
let __ () =
  let module M = Lists in
  [300; 500; (*700; 900; 1100*) ] |> List.iter (fun n ->
    let xs = M.create n in
    let wrap f () =
      (* Gc.major ();
      Gc.minor ();
      Gc.compact (); *)
      let _ = f () xs in
      ()
    in
    let res =
      throughputN ~repeat timeout
        [ (Printf.sprintf "%s_D_%d" M.name n, wrap M.A.fold, ())
        ; (Printf.sprintf "%s_V" M.name, wrap M.B.fold, ())
        ; (Printf.sprintf "%s_G" M.name, wrap M.C.fold, ())
        ]
    in
    print_newline();
    tabulate res
  )

let () =
  let module M = Trees in
  [ 10; 15; 20 (*700; 900*) ] |> List.iter (fun n ->
    let xs = M.create n in
    let wrap f () =
      (* Gc.major ();
      Gc.minor ();
      Gc.compact (); *)
      let _ = f () xs in
      ()
    in
    let res =
      throughputN ~repeat timeout
        [ (Printf.sprintf "%s_D_%d" M.name n, wrap M.A.fold, ())
        ; (Printf.sprintf "%s_V_%d" M.name n, wrap M.B.fold, ())
        ; (Printf.sprintf "%s_G_%d" M.name n, wrap M.C.fold, ())
        ]
    in
    print_newline();
    tabulate res
  )

let  () =
  let module M = ManyConstrs in
  [ 10; 15; 20 (* 700; 900*) ] |> List.iter (fun n ->
    let xs = M.create n in
    let wrap f () =
      (* Gc.major ();
      Gc.minor ();
      Gc.compact (); *)
      let _ = f () xs in
      ()
    in
    let res =
      throughputN ~repeat timeout
        [ (Printf.sprintf "%s_D_%d" M.name n, wrap M.A.fold, ())
        ; (Printf.sprintf "%s_V_%d" M.name n, wrap M.B.fold, ())
        ; (Printf.sprintf "%s_G_%d" M.name n, wrap M.C.fold, ())
        ]
    in
    print_newline();
    tabulate res
  )
