module M = struct
  exception (* EE => *) EE (* <= EE *)
end

let _ = raise M.EE (* ? EE *)
