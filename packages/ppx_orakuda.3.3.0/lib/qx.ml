open Spotlib.Spot

let command s = 
  let open Command in
  shell s |> stdout |> wait |> function
    | ls, ps -> ps, ls
