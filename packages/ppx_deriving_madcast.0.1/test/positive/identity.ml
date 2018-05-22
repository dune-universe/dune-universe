let () =
  assert ([%madcast: int -> int] 42 = 42) ;
  assert ([%madcast: char -> char] 'c' = 'c') ;
  assert ([%madcast: int list -> int list] [42; 0] = [42; 0]) ;
  assert ([%madcast: string -> string] "42" = "42") ;
  assert ([%madcast: int array -> int array] [|1; 2|] = [|1; 2|])
