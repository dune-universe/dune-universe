module Option = struct
  let bind e f = match e with
    | Some v -> f v
    | None -> None
  let return v = Some v
end

let () =
  let res = 
    let open Option in do_;
    x <-- begin match%m return 1 with
      | 1 -> return 2
      | _ -> assert false
    end;
    if x <> 2 then assert false else return ()
  in
  assert (res = Some ())
