
let basis64 = 0xcbf29ce484222325L
let prime64 = 0x100000001b3L


             

                
let checksum key =
  let n = ref basis64 in
  let nbits = Cstruct.len key in
  
  let rec aux i =
    if i < nbits then

      let bit = Cstruct.get_uint8 key i |> Int64.of_int in 
      n := Int64.mul (Int64.logxor !n bit) prime64;

      aux (i + 1) 
      
    else
      !n

  in
  aux 0 
        
      
let digest key =
  let buf = Cstruct.create 8 in 
  Cstruct.BE.set_uint64 buf 0 (checksum key);
  buf 
