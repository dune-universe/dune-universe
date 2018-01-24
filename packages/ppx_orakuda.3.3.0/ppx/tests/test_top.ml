let f () = let %top x = 1 in x
let g () = let module %top M = struct let y = 2 end in M.y

module M = struct
  let %top z = 1;;

  [%%top module N = struct let w = 4 end  ]
end

let x = (1+2)[@top]


