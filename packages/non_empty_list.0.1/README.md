# ocaml-non-empty-list
A list that is known, statically, to be nonempty. 
The `hd` and `tl` functions don't require `option` types, or exceptions, improving safety.  

This library follows closely Janestreet's `Base` library,
providing similar complexity and functions to `Base.List`. 

## Usage


```ocaml
open Non_empty_list;;

let one = [1];;
val one : int non_empty_list = (::) (1, []) (* [1] *)

let two = cons 2 one;;
val two : int non_empty_list = (::) (2, [1]) (* [2; 1] *)

hd one;;
- : int = 1

append one two;;
- : int non_empty_list = (::) (1, [2; 1]) (* [1; 2; 1] *)

length two;;
- : int = 2

to_list two;;
- : int list = [2; 1]

mem ~equal:( = ) two 2;;
- : bool = true

reduce ~f:( + ) two;;
- : int = 3
```

## Testing
```
dune build
dune runtest
```

