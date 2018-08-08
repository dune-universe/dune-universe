class x = object (self)
end

class y = object
  inherit x as super (* CR jfuruse: super has no loc info in OCaml 4.00.0 *)
end
