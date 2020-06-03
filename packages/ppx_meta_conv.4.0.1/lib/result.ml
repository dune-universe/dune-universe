(** Result monad *)
type ('a, 'error) t = ('a, 'error) result

let bind t f = match t with
  | Ok v -> f v
  | Error err -> Error err

let (>>=) = bind

let fmap f t = match t with
  | Ok v -> Ok (f v)
  | Error err -> Error err

let (>>|) t f = fmap f t

let map dec ts =
  let rec map st = function
    | [] -> Ok (List.rev st)
    | t::ts -> 
        match dec t with
        | Ok h -> map (h::st) ts
        | Error err -> Error err
  in
  map [] ts

let mapi dec ts =
  let rec map st n = function
    | [] -> Ok (List.rev st)
    | t::ts -> 
        match dec n t with
        | Ok h -> map (h::st) (n+1) ts
        | Error err -> Error err
  in
  map [] 0 ts

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

let catch_exn f = catch (fun ~fail -> try f () with e -> fail e)

let result okf errf = function
  | Ok v -> okf v
  | Error e -> errf e

module Open = struct
  let (>>=) = (>>=)
  let (>>|) = (>>|)
end
