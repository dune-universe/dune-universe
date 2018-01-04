open Base
open Stdio
open Parsexp

let load_exn (type a) (module P : Parser with type parsed_value = a) ~filename =
  In_channel.with_file filename ~f:(fun ic ->
    let buf = Bytes.create 8192 in
    let state = P.State.create () in
    let rec loop stack =
      match In_channel.input ic ~buf ~pos:0 ~len:(Bytes.length buf) with
      | 0 -> P.feed_eoi state stack
      | n -> loop (P.feed_subbytes state buf stack ~pos:0 ~len:n)
    in
    loop P.Stack.empty)

let load parser ~filename =
  match load_exn parser ~filename with
  | x                         -> Ok x
  | exception (Parse_error e) -> Error e

type ('a, 'b) conv_mode =
  | Single : ('a, 'a     ) conv_mode
  | Many   : ('a, 'a list) conv_mode

let load_conv_exn
  :  type a b. (a, b) conv_mode
    -> filename:string
  -> (Sexp.t -> a)
  -> b
  = fun mode ~filename f ->
    match mode with
    | Single -> Conv_single.conv_exn (load_exn (module Single_and_positions) ~filename) f
    | Many   -> Conv_many  .conv_exn (load_exn (module   Many_and_positions) ~filename) f

let load_conv
  :  type a b. (a, b) conv_mode
    -> filename:string
  -> (Sexp.t -> a)
  -> (b, Conv_error.t) Result.t
  = fun mode ~filename f ->
    match mode with
    | Single -> Conv_single.conv_combine (load (module Single_and_positions) ~filename) f
    | Many   -> Conv_many  .conv_combine (load (module   Many_and_positions) ~filename) f
