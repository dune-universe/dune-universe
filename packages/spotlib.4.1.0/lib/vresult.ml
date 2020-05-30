type ('a, 'error) t = ('a, 'error) result = Ok of 'a | Error of 'error

module M = Monad.Make2(struct
  type nonrec ('a, 'error) t = ('a, 'error) t

  let return v = Ok v

  let bind t f = match t with
    | Ok v -> f v
    | Error e -> Error e
end)

module Infix = struct
  include M.Infix

  let (>>=!) v f = match v with
    | Ok v -> Ok v
    | Error e -> f e
    
  let (>>|!) v f = match v with
    | Ok v -> Ok v
    | Error e -> Error (f e)
end

include M.S
include M.EX
include Infix
  
let fail e = Error e

let catch f = 
  let module Error = struct exception Error end in
  let error = ref None in
  let fail e = error := Some e; raise Error.Error in
  try Ok (f ~fail) with
  | Error.Error -> 
      match !error with
      | Some e -> Error e
      | None -> assert false

let catch_exn f = catch (fun ~fail -> try f () with e -> fail (`Exn e))

let map_error f = function
  | Ok v -> Ok v
  | Error e -> Error (f e)

let to_option = function
  | Ok v -> Some v
  | Error _ -> None

exception IsError

module Stdlib = struct

  let ok x = Ok x
  let ng x = Error x

  let from_Ok = function
    | Ok v -> v
    | Error _ -> raise IsError
  
  let result left right = function
    | Ok v -> left v
    | Error e -> right e

  let at_Error f = result (fun x -> x) f
end

include Stdlib
