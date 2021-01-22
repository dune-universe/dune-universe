open Question_type

let mkco =
  List.map (fun x -> "\\co{"^x^"}")
   
let qarray = [|
    mkqcm "quelles est le type de la fonction \\co{fun x -> x} ?"
          (mkco [ "int -> int"; "string -> string"; "'a -> int"; "'a -> 'a" ])
          D ;
    mkqcm "quelles est le type de la fonction \\co{fun x -> x+1} ?"
          (mkco [ "int -> int"; "string -> string"; "'a -> int"; "'a -> 'a" ])
          A;
    mkqcm "quelles est le type de la fonction \\co{fun x -> \"bonjour\"^x} ?"
          (mkco [ "int -> int"; "string -> string"; "'a -> int"; "'a -> 'a" ])
          B;
    mkqcm "soit \\co{f : 'a -> int} laquuelle de ces expression est non valide ?"
          (mkco [ "f 1"; "f (fun x -> x+1)"; "(1.0 +. (f 2)"; "elle sont toutes valide" ])
          C ;
    mkfun " \\co{i j} renvoie la distance entre i et j ."
          "int -> int -> int"
          "(fun i j -> abs (i-j))" [];
    mkfun "\\co{f g} renvoie la composition de f et g ."
          "(int -> int) -> (int -> int) -> int -> int"
          "(fun f g x -> f (g x))" [];
     mkqcm "qui a sauvé l'UPEC de l'invasion d'étudiant flemmard ?"
           [ "Jacquouille" ; "Le prof de système" ; "Obi-Wan Kenobi" ; "Coro Navir" ]
           D
  |]


let _ =
  save_qbank "testqbank" { questions =qarray ;
              digest = Digest.string @@ string_of_int @@ Hashtbl.hash qarray;
              shuffle = None
            }
