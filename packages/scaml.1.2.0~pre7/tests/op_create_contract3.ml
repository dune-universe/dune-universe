open SCaml

let [@entry] main () () =
  let op, ad = 
    (* Write Michelson contract code in a string.  
       I recommend to use OCaml's quoted string {| .. |}
       to avoid escaping newlines and double quotes.
    *)
    Contract.create_from_tz_file "sample.tz" None (Tz 1.0) ()
  in
  [op], ()

