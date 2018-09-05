let (>>=) value f =
  match value with
  | Result.Ok result -> f result
  | Result.Error _ as error -> error

let (>|=) value f =
  match value with
  | Result.Ok result -> Result.Ok (f result)
  | Result.Error _ as error -> error
