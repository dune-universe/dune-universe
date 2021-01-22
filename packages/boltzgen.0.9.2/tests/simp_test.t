Simple test on standard lib types

  $ cat >reference.ml <<EOF
  > let sum x y = x + y
  > let sum2 x y = x +. y
  > let app f x = f x
  > let rec fact n = if n < 1 then 1 else n * fact (n - 1)
  > let map = List.map
  > let concat l1 l2 = l1 @ l2
  > let greating x = "hello:"^x
  > let correct = List.fold_left
  > EOF

  $ cat >wrong.ml <<EOF
  > let sum x y = x - y
  > let sum2 = sum
  > let app f x = f x +. 1.0
  > let rec fact n = if n = 1 then n else n * fact (n - 1)
  > let map f l = []
  > let greating x = "hello "^x
  > let correct = List.fold_left
  > EOF

  $ boltzgen "val sum: int -> int->int" wrong.ml reference.ml --seed 42
  sum 0 (-1) = 1 instead of -1
  [1]

  $ boltzgen "val sum2: float -> float->float" wrong.ml reference.ml --seed 42
  File "./t.ml", line 34, characters 29-34:
  34 | module TA = TestFunctorDiff (Wrong) (Reference) ;;
                                    ^^^^^
  Error: Signature mismatch:
         ...
         Values do not match:
           val sum2 : int -> int -> int
         is not included in
           val sum2 : float -> float -> float
         File "./t.ml", line 3, characters 1-35: Expected declaration
         File "./wrong.ml", line 2, characters 4-8: Actual declaration
  [2]

  $ boltzgen "val app: ('a -> float) -> 'a -> float" wrong.ml reference.ml --seed 42
  app (fun_a 1) 0 = -16.0469817942 instead of -17.0469817942
  [1]

  $ boltzgen "val fact: nat -> int" wrong.ml reference.ml --seed 42
  fact 0 = Exception occurs instead of 1
  [1]

  $ boltzgen "val map: ('a -> 'b) -> 'a list -> 'b list" wrong.ml reference.ml --seed 42
  map (fun_a 1) [1; 1; 0] = [] instead of [-9; -9; 1]
  [1]

  $ boltzgen "val concat: 'a list -> 'a list -> 'a list" wrong.ml reference.ml --seed 42      
  File "./t.ml", line 34, characters 29-34:
  34 | module TA = TestFunctorDiff (Wrong) (Reference) ;;
                                    ^^^^^
  Error: Signature mismatch:
         ...
         The value `concat' is required but not provided
         File "./t.ml", line 3, characters 1-43: Expected declaration
  [2]

  $ boltzgen "val greating: id_string -> string" wrong.ml reference.ml --seed 42        
  greating "dbt" = "hello dbt" instead of "hello:dbt"
  [1]

  $ boltzgen "val correct: ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a" wrong.ml reference.ml -b 10 --seed 42                


Tests with custom types

  $ cat >reference.ml <<EOF
  > type 'a tree = Empty | Node of 'a tree * 'a * 'a tree
  > let rec size = function Empty -> 0 | Node (fg,_,fd) -> 1+(size fg) +(size fd)
  > let rec flatten acc = function Empty -> acc | Node (fg,v,fd) -> flatten (v @ (flatten acc fg)) fd 
  > EOF

  $ cat >wrong.ml <<EOF
  > type 'a tree = Empty | Node of 'a tree * 'a * 'a tree
  > let rec size = function Empty -> 0 | Node (fg,_,fd) -> 1+ (max (size fg) (size fd))
  > let rec flatten acc = function Empty -> acc | Node (fg,v,fd) -> v@acc 
  > EOF

  $ boltzgen "type 'a tree = Empty | Node of 'a tree * 'a * 'a tree val size: 'a tree -> int" wrong.ml reference.ml -b 10 --seed 42 
  size (Node(((Node((Empty,1,Empty))),0,(Node((Empty,0,(Node((Empty,2,Empty))))))))) = 3 instead of 4
  [1]

  $ boltzgen "type 'a tree = Empty | Node of 'a tree * 'a * 'a tree val flatten: string list -> string list tree -> string list" wrong.ml reference.ml -b 10 --seed 42 
  flatten [""; "<'"; ""; ""] (Node(((Node(((Node((Empty,[],(Node(((Node((Empty,[""; ""],Empty))),[],Empty)))))),["2R"; "}="; ""; ""],(Node((Empty,[],Empty)))))),[""],Empty))) = [""; ""; "<'"; ""; ""] instead of [""; "2R"; "}="; ""; ""; ""; ""; ""; "<'"; ""; ""]
  [1]
