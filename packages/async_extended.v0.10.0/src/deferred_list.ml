open Core
open Async

type how = [`parallel|`sequential]

let iter how l ~f =
  match how with
  | `sequential ->
    Deferred.create (fun ivar ->
      let rec loop l =
        match l with
        | [] -> Ivar.fill ivar ()
        | hd :: tl -> f hd >>> fun () -> loop tl
      in
      loop l)
  | `parallel ->
    Deferred.all_unit (List.map l ~f)

let map how l ~f =
  match how with
  | `sequential ->
    Deferred.create (fun ivar ->
      let rec loop l accum =
        match l with
        | [] -> Ivar.fill ivar (List.rev accum)
        | hd :: tl -> f hd >>> fun res -> loop tl (res :: accum)
      in
      loop l [])
  | `parallel ->
      Deferred.all (List.map l ~f)

let filter_map how l ~f =
  map how l ~f >>|
  List.filter_opt

