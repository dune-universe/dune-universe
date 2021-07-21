include Biotk_pipes.Pipe.S with type 'a monad = 'a

val from_file : ?buffer_size:int -> string -> string source

val to_file : string -> string sink
