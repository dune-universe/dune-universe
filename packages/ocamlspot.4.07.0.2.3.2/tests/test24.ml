module Module = struct
  type (* Module.t => *) t (* <= Module.t *) = Foo (* (hopefully old) p4 location handling bug *)
end

type t = Zoo of Module.t (* ? Module.t *) 
