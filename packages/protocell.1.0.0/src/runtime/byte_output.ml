type t = Buffer.t

let create ?(initial_size = 1024) () = Buffer.create initial_size

let write_byte = Buffer.add_char

let write_bytes = Buffer.add_string

let write_bytes' = Buffer.add_bytes

let contents = Buffer.contents
