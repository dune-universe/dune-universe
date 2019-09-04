module OCSS = OCamlStandard.Stream

open Reference.O
open Int.O
open Functions.Function1.O

type 'a t = 'a OCSS.t

let empty = OCSS.sempty

let of_list = OCSS.of_list

let singleton = OCSS.ising

let to_list xs =
  let ys = ref [] in
  OCSS.iter (fun x ->
    ys := x::!ys
  ) xs;
  List.reverse !ys

let concat = OCSS.iapp

let prepend = OCSS.icons

let try_next xs =
  OCSS.peek xs
  |> Option.map ~f:(fun x ->
    OCSS.junk xs;
    x
  )


#define FILTER_MAP_I_ACC(NAME) \
let NAME xs ~f = \
  let aux _ = \
    CONCAT(NAME, _next) xs ~f \
  in \
  OCSS.from aux \
\
let CONCAT(NAME, _acc) ~acc xs ~f = \
  let acc = ref acc in \
  let aux _ = \
    CONCAT(NAME, _next) xs ~f:(fun x -> \
      let (acc', y) = f ~acc:!acc x in \
      acc := acc'; \
      y \
    ) \
  in \
  OCSS.from aux \
\
let CONCAT(NAME, _i) xs ~f = \
  CONCAT(NAME, _acc) ~acc:0 xs ~f:(fun ~acc:i x -> (i + 1, f ~i x))


let map_next xs ~f =
  OCSS.peek xs
  |> Option.map ~f:(fun x ->
    OCSS.junk xs;
    f x
  )

FILTER_MAP_I_ACC(map)


let filter_next xs ~f =
  let rec aux () =
    OCSS.peek xs
    |> Option.filter_map ~f:(fun x ->
      OCSS.junk xs;
      if f x then
        Some x
      else
        aux ()
    )
  in
  aux ()

FILTER_MAP_I_ACC(filter)


let filter_map_next xs ~f =
  let rec aux () =
    OCSS.peek xs
    |> Option.filter_map ~f:(fun x ->
      OCSS.junk xs;
      match f x with
        | None -> aux ()
        | y -> y
    )
  in
  aux ()

FILTER_MAP_I_ACC(filter_map)

#undef FILTER_MAP_I_ACC


let flat_map xs ~f =
  let current = ref None in
  let rec aux n =
    if Option.is_none !current then current := map_next xs ~f;
    match !current with
      | None -> None
      | Some ys -> begin
        match OCSS.peek ys with
          | None -> current := None; aux n
          | x -> OCSS.junk ys; x
      end
  in
  OCSS.from aux

let flat_map_acc ~acc xs ~f =
  let acc = ref acc
  and current = ref None in
  let rec aux n =
    if Option.is_none !current then current := map_next xs ~f:(fun x -> let (acc', y) = f ~acc:!acc x in acc := acc'; y);
    match !current with
      | None -> None
      | Some ys -> begin
        match OCSS.peek ys with
          | None -> current := None; aux n
          | x -> OCSS.junk ys; x
      end
  in
  OCSS.from aux

let flat_map_i xs ~f =
  flat_map_acc ~acc:0 xs ~f:(fun ~acc:i x -> (i + 1, f ~i x))


module ToList = struct
  let map xs ~f =
    let rec aux ys =
      match try_next xs with
        | None -> List.reverse ys
        | Some x -> aux ((f x)::ys)
    in
    aux []

  let map_acc ~acc xs ~f =
    let rec aux acc ys =
      match try_next xs with
        | None -> List.reverse ys
        | Some x ->
          let (acc, y) = f ~acc x in
          aux acc (y::ys)
    in
    aux acc []

  let map_i xs ~f =
    map_acc ~acc:0 xs ~f:(fun ~acc:i x -> (i + 1, f ~i x))


  let filter xs ~f =
    let rec aux ys =
      match try_next xs with
        | None -> List.reverse ys
        | Some x ->
          let ys = if f x then x::ys else ys in
          aux ys
    in
    aux []

  let filter_acc ~acc xs ~f =
    let rec aux acc ys =
      match try_next xs with
        | None -> List.reverse ys
        | Some x ->
          let (acc, b) = f ~acc x in
          let ys = if b then x::ys else ys in
          aux acc ys
    in
    aux acc []

  let filter_i xs ~f =
    filter_acc ~acc:0 xs ~f:(fun ~acc:i x -> (i + 1, f ~i x))


  let filter_map xs ~f =
    let rec aux ys =
      match try_next xs with
        | None -> List.reverse ys
        | Some x ->
          let ys =
            match f x with
              | Some y -> y::ys
              | None -> ys
          in
          aux ys
    in
    aux []

  let filter_map_acc ~acc xs ~f =
    let rec aux acc ys =
      match try_next xs with
        | None -> List.reverse ys
        | Some x ->
          let (acc, y) = f ~acc x in
          let ys =
            match y with
              | Some y -> y::ys
              | None -> ys
          in
          aux acc ys
    in
    aux acc []

  let filter_map_i xs ~f =
    filter_map_acc ~acc:0 xs ~f:(fun ~acc:i x -> (i + 1, f ~i x))


  let flat_map xs ~f =
    let rec aux ys =
      match try_next xs with
        | None -> List.reverse ys
        | Some x -> aux (List.rev_concat (f x) ys)
    in
    aux []

  let flat_map_acc ~acc xs ~f =
    let rec aux acc ys =
      match try_next xs with
        | None -> List.reverse ys
        | Some x ->
          let (acc, y) = f ~acc x in
          aux acc (List.rev_concat y ys)
    in
    aux acc []

  let flat_map_i xs ~f =
    flat_map_acc ~acc:0 xs ~f:(fun ~acc:i x -> (i + 1, f ~i x))
end
