type t = A of GT.int [@@deriving gt ~options:{show}]

let () = Format.printf "Should be an ADT:   `%s`\n%!" (GT.show t (A 5))

(* Let's override implementation to use fancy printing *)
let t =
  {
    GT.gcata = gcata_t;
    GT.fix = (fun eta -> GT.transform_gc gcata_t eta);
    GT.plugins = (object method show (A n) = string_of_int n end)
  }

let () = Format.printf "Should be a number: `%s`\n%!" (GT.show t (A 5))

(* By default t2 as being type abbreviation constructed in combinatorial manner
  (by not using class for type t *)
type t2 = t [@@deriving gt ~options:{show}]
let () = Format.printf "Should be a number: `%s`\n%!" (GT.show t2 (A 5))
