[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main param storage =
  [],
  assert (
    (* XXX tuple is formed, which is redundant *)
    ( match (Left (Int 1) : (int, unit) sum) with
      | Left x as y -> 
          (* or type is uncomparable!!!
             (Left x : (_, unit) sum) = y  
          *)
          begin match (y : (int, unit) sum) with
            | Left y -> x = y
            | _ -> false
          end
      | _ -> false
    )
  )
