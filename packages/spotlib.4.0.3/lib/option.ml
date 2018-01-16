type 'a t = 'a option

module M = Monad.Make(struct
  type 'a t = 'a option

  let return v = Some v

  let bind t f = match t with
    | Some v -> f v
    | None -> None
end)

module Infix = struct
  include M.Infix

  let (>>=!) v f = match v with
    | Some _ -> v
    | None -> f ()

  let (//) t v = match t with
    | None -> v
    | Some v -> v
end
  
include M.S
include M.EX
include Infix

let iter f = function
  | None -> ()
  | Some v -> f v

let default v df = match v with
  | None -> df ()
  | Some v -> v

let catch f =
  let module Error = struct exception Error end in
  let fail () = raise Error.Error in
  try Some (f ~fail) with Error.Error -> None

let to_result = function
  | Some v -> Ok v
  | None -> Error `None

let to_poly_result = function
  | Some v -> `Ok v
  | None -> `Error `None

let catch_exn f = catch (fun ~fail -> try f () with _ -> fail ())

let format f ppf = function
  | None -> Format.pp_print_string ppf "None"
  | Some v -> Format.fprintf ppf "Some (%a)" f v

let from_Some = function
  | Some v -> v
  | _ -> invalid_arg "Option.from_Some"

let to_list = function
  | None -> []
  | Some x -> [x]

module Pervasives = struct
  let from_Some = from_Some
  let (//) = (//)
end


