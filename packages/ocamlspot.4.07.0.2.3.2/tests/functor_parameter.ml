module F((* A => *) A  (* <= A *): sig val x : int end) = struct
  let y = A.x (* ? A *)
end
