val with_resource :
  acquire:('a -> 'b) -> 'a -> ('b -> 'c) -> release:('b -> unit) -> 'c
