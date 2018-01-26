let untyped_identity = { Ast_mapper.default_mapper with
  structure = (fun _ x -> x);
  signature = (fun _ x -> x)
}

module Typemod = Typemod
  
module Typed_identity = struct
  let map_structure x = x
  let map_signature x = x
end
