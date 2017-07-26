module type Pipe = sig
  type t
  type reader
  type writer
  val create : unit -> reader * writer
  val write : t -> writer -> unit Lwt.t
  val write_with_pushback : t -> writer -> unit Lwt.t
  val read : reader -> t Lwt.t
end

module Make(Material : sig type t end) : (Pipe with type t = Material.t) = struct
  open Lwt.Infix

  type t = Material.t

  type reader = Lwt_io.input_channel
  type writer = Lwt_io.output_channel
  type pushback = Empty | WithChannel of Lwt_io.output_channel

  let create () = Lwt_io.pipe ()

  let write v chan =
    Lwt_io.write_value ~flags:[Marshal.Closures] chan (v, Empty)

  let write_with_pushback v chan =
    let (i, o) = Lwt_io.pipe () in
    Lwt_io.write_value ~flags:[Marshal.Closures] chan (v, WithChannel o) >>= fun _ ->
    Lwt_io.read_value i >>= fun _ ->
    Lwt.return ()

  let read chan =
    Lwt_io.read_value chan >>= fun (v, pushback) ->
    match pushback with
    | Empty ->
        Lwt.return v
    | WithChannel o ->
        Lwt_io.write_value o v >>= fun _ -> Lwt.return v
end

module BoolPipe = Make(struct type t = bool end)
module StringPipe = Make(struct type t = string end)
module IntPipe = Make(struct type t = int end)
module CharPipe = Make(struct type t = char end)
