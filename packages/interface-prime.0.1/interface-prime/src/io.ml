module type S_base = sig
  module M : Monad.S
  type in_channel
  type out_channel
  val read_exactly : in_channel -> int -> string option M.t
  val read_available : in_channel -> string M.t
  val write : out_channel -> string -> unit M.t
  val close_in : in_channel -> unit M.t
  val close_out : out_channel -> unit M.t
end

module type S = sig
  include S_base
  val read_byte : in_channel -> char option M.t
  val read_int32 : in_channel -> int32 option M.t
  val read_int64 : in_channel -> int64 option M.t
  val write : out_channel -> string -> unit M.t
end

(*
module Make(Io : S_base) : S with type in_channel = Io.in_channel and type out_channel = Io.out_channel = struct
  include Io
  open Io.M

  let buffer = ref Bytes.empty

  let read_byte ic = read_exactly ic 1 >|= (fun s -> if String.length s == 0 then None else Some s.[0])

  let read_int32 ic = read_exactly ic 4
    >|= fun s ->
        ((s.[0] |> int_of_char |> Int32.of_int |> Int32.shift_left) 24)
        |> Int32.add
        ((s.[1] |> int_of_char |> Int32.of_int |> Int32.shift_left) 16)
        |> Int32.add
        ((s.[2] |> int_of_char |> Int32.of_int |> Int32.shift_left) 8)
        |> Int32.add
        (s.[3] |> int_of_char |> Int32.of_int)

  let read_int64 ic = read_exactly ic 8
    >|= fun s ->
      ((s.[0] |> int_of_char |> Int64.of_int |> Int64.shift_left) 56)
      |> Int64.add
      ((s.[1] |> int_of_char |> Int64.of_int |> Int64.shift_left) 48)
      |> Int64.add
      ((s.[2] |> int_of_char |> Int64.of_int |> Int64.shift_left) 40)
      |> Int64.add
      ((s.[3] |> int_of_char |> Int64.of_int |> Int64.shift_left) 32)
      |> Int64.add
      ((s.[4] |> int_of_char |> Int64.of_int |> Int64.shift_left) 24)
      |> Int64.add
      ((s.[5] |> int_of_char |> Int64.of_int |> Int64.shift_left) 16)
      |> Int64.add
      ((s.[6] |> int_of_char |> Int64.of_int |> Int64.shift_left) 8)
      |> Int64.add
      (s.[7] |> int_of_char |> Int64.of_int)
end
*)
