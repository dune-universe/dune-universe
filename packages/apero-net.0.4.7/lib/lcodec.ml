open Locator
open Apero


let decode_locator buf = 
  let s = decode_string buf in
  Locator.of_string s

let encode_locator l = 
  encode_string (Locator.to_string l) 

let decode_locators buf =   
  let ols = decode_seq decode_locator buf in 
  let ls = Option.get @@ Option.flatten ols in 
  Locators.of_list ls
  
let encode_locators ls buf = 
  let locs = Locators.to_list ls in
  encode_seq encode_locator locs buf
