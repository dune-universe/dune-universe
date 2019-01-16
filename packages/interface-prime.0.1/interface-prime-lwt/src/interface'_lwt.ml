module Monad_base = struct
  type 'a t = 'a Lwt.t
  let return = Lwt.return
  let bind = Lwt.bind
end

module Monad = Interface'.Monad.Make(Monad_base)


module Monad_result(E : Interface'.Monad.ErrorType) = Interface'.Monad_result.Make(Monad)(E)

(*module Make_io(M : Interface'.Monad.S with type +'a t = 'a Lwt.t) = struct
  module M = M
  open M
  type in_channel = Lwt_io.input_channel
  type out_channel = Lwt_io.output_channel
  let read ic count =
    let bytes = Bytes.create count in
    Lwt_io.read_into_exactly ic bytes 0 count
    >> (bytes |> Bytes.to_string |> return)
end
*)

module Io : Interface'.Io.S with module M = Monad and type in_channel = Lwt_io.input_channel and type out_channel = Lwt_io.output_channel = struct
  module M = Monad
  open M
  type in_channel = Lwt_io.input_channel
  type out_channel = Lwt_io.output_channel
  let read_exactly ic =
    let handle_exn = function
      | End_of_file -> None |> return
      | e ->
        e |> Printexc.to_string |> Lwt_io.printf "Failed to read: [%s]\n"
        >> lazy (None |> return)
    in function
      | 1 ->
          Lwt.catch
            (fun _ ->
              Lwt_io.read_char ic >|= (String.make 1) >|= (fun x -> Some x))
            handle_exn
      | count ->
        try
          let bytes = Bytes.create count in
            Lwt.catch
              (fun _ ->
                Lwt_io.read_into_exactly ic bytes 0 count
                >> lazy (bytes |> Bytes.to_string |> return)
                >|= (fun x -> Some x))
              handle_exn
        with _ -> return None

  let read_available ic =
    Lwt_io.read ic

  let read_byte = Lwt_io.read_char_opt

  let read_int32 ic = try Lwt_io.read_int32 ic >|= (fun x -> Some x) with _ -> return None

  let read_int64 ic = try Lwt_io.read_int64 ic >|= (fun x -> Some x) with _ -> return None

  let write = Lwt_io.write

  let close_in = Lwt_io.close

  let close_out = Lwt_io.close
end
