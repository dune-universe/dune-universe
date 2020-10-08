@type 'l t = 
    R 
  | W 
  | L  of string 
  | S  of string 
  | B  of (int -> int -> int) * string
  | E
  | C  of int
  | J  of 'l
  | JT of 'l
  | JF of 'l
