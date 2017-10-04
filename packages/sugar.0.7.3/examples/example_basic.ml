

(* Example of basic usage of Sugar to build an Error Handling Layer
 *
 * It uses a parametrized module to build sugar syntatic and useful functions
 * that only make sense in your project.
 *
 * We're hinting the functions result to make clear what's going on. But you
 * can effectively remove them, because we're also using a concise result type
 * for computations: ('a result).
 *)

(* Create a module with your own "error" type *)
module MyError =
struct
  type t
    = Resource_not_found
    | Unexpected of string
end

(* Generate your error handling layer with your parametrized Result module *)
module MyResult = Sugar.Result.Make (MyError)

(* Start using them *)
open MyResult
open MyError
open Printf

(* We're only printing messages to the screen here *)
let puts m: unit result =
  print_endline m;
  return ()

(* Do some computation and return a list, if it is successful *)
let load_list n: int list result =
  let l = [1; 2; 3] in
  let new_list = List.map (fun v -> v * n) l in
  return new_list

let error_handler e: string result =
  match e with
  | Resource_not_found -> return "recovered failure"
  | _ -> throw e

open MyResult.Infix

let _ =
  print_endline "Hello World"


let _ =
  puts "We are extensively using a user defined result type" >>lazy
  ( List.length <$> load_list 10 )
  >>=
  ( fun len ->
    throw (Unexpected (sprintf "List length invalid: %d" len))
  )
  >---------
  ( function
    e -> error_handler e
  )
  >>=
  ( fun recovered ->
    ( puts "This will NOT be printed"        ) >>lazy
    ( puts "The previous error_handler"      ) >>lazy
    ( puts "can't catch 'Unexpected' errors" ) >>lazy
    ( return "for sure" )
  )
  >---------
  ( fun e ->
    return "Recover from any error"
  )
  >>= puts


let _ =
  puts "We are extensively using a user defined result type" >>lazy
  ( load_list 10 )
  >>| List.length
  >>=
  ( fun len ->
    throw (Unexpected (sprintf "List length invalid: %d" len))
  )
  >---------
  ( function
    e -> error_handler e
  )
  >>=
  ( fun recovered ->
    ( puts "This will NOT be printed"        ) >>lazy
    ( puts "The previous error_handler"      ) >>lazy
    ( puts "can't catch 'Unexpected' errors" ) >>lazy
    ( return "for sure" )
  )
  >---------
  ( fun e ->
    return "Recover from any error"
  )
  >>= puts
