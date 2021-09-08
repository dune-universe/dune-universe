
let to_string c = Printf.sprintf "%c" c

module Manager =
  UtilsLib.DependencyManager.Make(struct
			  type t = char
			  let compare=compare
			  let _to_string=to_string
			end)

let manager = Manager.empty

let manager =
  List.fold_left
    (fun man (c1,c2) -> Manager.add_dependency c1 c2 man)
    manager
    ['j','l';
     'j','f';
     'l','h';
     'h','g';
     'h','e';
     'i','f';
     'i','g';
     'k','f';
     'k','a';
     'k','m';
     'f','b';
     'f','c';
     'g','c';
     'g','d';
     'h','e';
     'e','m';
     'n','k';
     'n','h';
     'o','n';
     'o','j';
    ]




let test elt = 
  let () = UtilsLib.Log.set_level ~app:"test_dependencyManager" Logs.Warning in
  let lst = Manager.dependencies elt manager in
  let () = Logs.app (fun m -> m  "An appropriate dependency list from %c is:" elt) in
  let () = Logs.app (fun m -> m  "%s" (UtilsLib.Utils.string_of_list " " to_string lst)) in
  Logs.app (fun m -> m "Done.")


let () = List.iter test ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o']


let () = Logs.app (fun m -> m "The list of roots is: %s" (UtilsLib.Utils.string_of_list " " to_string (Manager.roots manager)))
