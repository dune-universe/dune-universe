let (>>=) result fn =
  match result with
  | Ok result -> fn result
  | error -> error
