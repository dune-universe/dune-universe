include Typpx.Make.F(struct
  let tool_name = "ppx_overload"
  let args = []
  let firstUntypedTransformation = Sugar.extend Ast_mapper.default_mapper
  module Typemod = Typpx.Default.Typemod
  module TypedTransformation = Mod.Map
  let lastUntypedTransformation = Typpx.Default.untyped_identity
end)
