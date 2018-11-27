type result =
  | Ok
  | Failed_checks

val run : out_channel -> Syntax_tree.specification -> result
