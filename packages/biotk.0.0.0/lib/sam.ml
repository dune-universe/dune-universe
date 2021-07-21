open Core_kernel

type header = Biocaml_unix.Sam.header
type alignment = Biocaml_unix.Sam.alignment

let fold fn ~init ~f =
  let open Or_error.Monad_infix in
  In_channel.with_file fn ~f:(fun ic ->
      Biocaml_unix.Sam.read ic >>= fun (header, alignments) ->
      let f = f header in
      CFStream.Stream.Or_error.fold' alignments ~init ~f
    )
