module M = struct 
  type t = Foo
  let f Foo = 1
end

module N = struct
  type t = Bar
  let g Bar = 2
end
