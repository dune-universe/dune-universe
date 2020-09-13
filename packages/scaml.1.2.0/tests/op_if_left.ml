[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main x y = 
  ([], 
   assert 
     ((match (Left (Int 1) : (int, unit) sum) with
         | Left x -> x = Int 1
         | Right x ->false)
      &&
      (match (Right (Int 2) : (unit, int) sum) with
       | Left x -> false
       | Right x -> x = Int 2)))
  

     
    

