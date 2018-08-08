module (* M => *) M (* <= M *) = struct
  let (* M.bar => *) bar (* <= M.bar *) = 42
end

include M (* ? M *)
let _ = bar (* ? M.bar *)
