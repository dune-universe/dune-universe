include Typpx.Make.F(struct
  let tool_name = "ppx_curried_constr"
  let args = []
  let firstUntypedTransformation = Typpx.Default.untyped_identity
  module Typemod = My_typemod
  module TypedTransformation = Typpx.Default.Typed_identity
  let lastUntypedTransformation = Typpx.Default.untyped_identity
end)


