[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main param storage =
  [],
  assert (
    ( match (Left (Int 1) : (int, unit) sum) with
      | Left x -> Int 1 = x
      | Right y -> false 
    )

    && 

    ( match (Left (Int 1) : (int, (int, unit) sum) sum) with
      | Left x -> Int 1 = x
      | Right (Left y) -> false 
      | Right (Right z) -> false 
    )
 
   && 

    ( match (Left (Int 1) : (int, (int, unit) sum) sum), Int 1 with
      | Left x, y -> x = y
      | Right (Left y), _ -> false 
      | Right (Right z), _ -> false 
    )

   && 

    ( match (Left (Int 1) : (int, (int, unit) sum) sum), (Int 1, Int 2) with
      | Left x, (y, _) -> x = y
      | Right (Left _), (x,y) -> x = y
      | Right (Right z), _ -> false 
    )

   && 

   ( match () with
     | () -> true
   )

   &&

   ( match (Left (Int 1) : (int, (int, unit) sum) sum) with
     | Left _ -> true
     | _ -> false
   )

   &&

   ( match true with
     | true -> true
     | _ -> false
   )

   && 

   ( match Int 1 with
     | Int 2 -> false
     | Int 1 -> true
     | _ -> false
   )

   && 

   ( match Nat 1 with
     | Nat 2 -> false
     | Nat 1 -> true
     | _ -> false
   )

   && 

   ( match "hello" with
     | "world" -> false
     | "hello" -> true
     | _ -> false
   )

   && 

   ( match Key_hash "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" with
     | Key_hash "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" -> true
     | _ -> false
   )

   && 

   ( match Timestamp "2019-09-11T08:30:23Z" with
     | Timestamp "2019-09-12T08:30:23Z" -> false
     | Timestamp "2019-09-11T08:30:23Z" -> true
     | _ -> false
   )

   && 

   ( match Address "tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" with
     | Address "tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" -> true
     | _ -> false
   )

   && 

   ( match Bytes "0123456789abcdef" with
     | Bytes "123456789abcdef0" -> false
     | Bytes "0123456789ABCDEF" -> true
     | _ -> false
   )
 )
