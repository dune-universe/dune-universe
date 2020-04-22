open Lambda;;
open L;;
open Parse;;
open Printf;;

let rec iloop () =
  printf "λ ← %!"; 
  let l = read_line () in
  (try 
    let t = parse l in
    let reduced = reduce_fix t in
    printf "λ → %s\n%!" (to_string reduced)
  with 
  | InvalidLambdaString -> printf "λ ⇸ error parsing lambda string\n%!"
  | _ -> printf "λ ⇸ error\n%!");
  iloop ()
;;

let () = match Array.length Sys.argv with
| _ -> iloop ();
(* | 2 -> let fp = Sys.argv.(1) in () *)
(* | _ -> printf "usage: lambda_i [file.lmd]\n"; *)
