module (* M => *) M (* <= M *) = struct
  let (* x => *) x (* <= x *) = 1
end

let _ = 
  let open M (* ? M *) (* damn, no position for M *) in
  x (* ? x *)
;;
