module Option = struct
  let bind e f = match e with
    | Some v -> f v
    | None -> None
  let return v = Some v
end

let () =
  let res = 
    let open Option in
    let%m a = prerr_endline "a"; None in (* printed *)
    let%m b = prerr_endline "b"; None in (* not printed *)
    prerr_endline "c"; return (a+b)  (* not printed *)
  in
  assert (res = None)

let () =
  let res = 
    let open Option in
    let%m x = prerr_endline "x"; None (* printed *)
    and   y = prerr_endline "y"; None (* printed *)
    in
    prerr_endline "z"; return (x+y)  (* not printed *)
  in
  assert (res = None)

  
