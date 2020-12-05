

let r1 = [ 1; 2; 3; 4]

let r2 = [ 5;6]

let r2' = []

let r3 = [7;8]


let a = [r1;r2;r3]


let string_of_res res =
  Printf.sprintf
    "%s"
    (List.fold_left (fun acc s -> Printf.sprintf "%d %s" s acc) "" res)

let rec string_of_array = function
  | [] -> ""
  | r::rows -> 
    Printf.sprintf
      "%s\n%s"
      (string_of_res (List.rev r))
      (string_of_array rows)
      
      
let ()=Printf.printf "r1: %s\n" (string_of_res r1)
  
let ()=Printf.printf "a=\n%s\n" (string_of_array a)

module AllIntArray = DatalogLib.ArrayTraversal.Make(
  struct
    type state=int list
    type cell=int
    let cell_compare i j=i-j
    let update s c = Some (c :: s)
  end
)

module EvenIntArray = DatalogLib.ArrayTraversal.Make(
  struct
    type state=int list
    let cell_compare i j=i-j
    type cell=int
    let update s c = if (c mod 2)=0 then Some (c :: s) else None
  end
)
module OddIntArray = DatalogLib.ArrayTraversal.Make(
  struct
    type state=int list
    let cell_compare i j=i-j
    type cell=int
    let update s c = if (c mod 2)=1 then Some (c :: s) else None
  end
)
  
let () =
  AllIntArray.collect_results
    (fun _ res -> Printf.printf "State: %s\n%!" (string_of_res res))
    ()
    []
    a

let () = print_newline()
  
let () =
  EvenIntArray.collect_results
    (fun _ res -> Printf.printf "State: %s\n%!" (string_of_res res))
    ()
    []
    a
let () = print_newline()

let () = 
  let () = UtilsLib.Log.set_level "test" Logs.Debug in
  OddIntArray.collect_results
    (fun _ res -> Printf.printf "State: %s\n%!" (string_of_res res))
    ()
    []
    a




	
