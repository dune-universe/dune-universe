open Grammar

let (>>=) result fn =
  match result with
  | Ok result -> fn result
  | error -> error

let closure_to_list c =
  Closure.fold (fun s e acc -> Cons (Cons (String s, e), acc)) c Nil
