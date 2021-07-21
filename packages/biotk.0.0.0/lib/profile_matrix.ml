open Gg
open Core_kernel
open Misc
open Biotk_croquis

module type S = sig
  type t = private float array array
  val of_array : float array array -> t option
  val flat : int -> t
  val length : t -> int
  val composition : t -> float array
  val draw : t -> Croquis.Picture.t
  val entropy : t -> float array
end

module type Alphabet = sig
  type t
  val all : t list
  val card : int
  val to_char : t -> char
  val of_char : char -> t option
  val of_char_exn : char -> t
  val to_int : t -> int
end

module Make(A : Alphabet) = struct
  type t = float array array

  let length = Array.length

  let flat n =
    let eps = 1. /. float A.card in
    Array.init n ~f:(fun _ ->
        Array.create ~len:A.card eps
      )

  let of_array = function
    | [||] -> None
    | xs ->
      if Array.for_all xs ~f:(fun x -> Array.length x = A.card)
      then Some xs
      else None

  let composition profile =
    let weights = Array.init A.card ~f:(fun j ->
        sum (Array.length profile) ~f:(fun i -> profile.(i).(j))
      ) in
    let total = Array.fold weights ~init:0. ~f:( +. ) in
    Array.map weights ~f:(fun w -> w /. total)

  let xlnx x =
    if Float.(x = 0.) then 0. else x *. Float.log x /. Float.log 2.

  let column_entropy p =
    -. sum (Array.length p) ~f:(fun j -> xlnx p.(j))

  let entropy = Array.map ~f:column_entropy

  let max_entropy = Float.log (float A.card) /. Float.log 2.

  let draw_y_scale () =
    let open Croquis.Picture in
    let text x y msg =
      text ~x ~y msg ~font:Croquis.Font.dejavu_sans_mono_bold ~size:0.1
    in
    blend [
      path [ 0., 0. ; 0., max_entropy ] ;
      path [ -. 0.1, 0. ; 0., 0. ] ;
      path [ -. 0.1, max_entropy /. 2. ; 0., max_entropy /. 2. ] ;
      path [ -. 0.1, max_entropy ; 0., max_entropy ] ;
      text (-. 0.2) 0. "0" ;
      text (-. 0.2) (max_entropy /. 2.) (Float.to_string (max_entropy /. 2.)) ;
      text (-. 0.2) max_entropy (Float.to_string max_entropy) ;
    ]

  let draw t =
    let open Croquis in
    let open Picture in
    let font = Font.dejavu_sans_mono_bold in
    let letter =
      List.map A.all ~f:(fun c ->
          sprintf "%c" (Char.uppercase (A.to_char c))
        )
      |> Array.of_list
    in
    let color =
      if A.card = 4 then
        Color.[| red ; blue ; v_srgbi 0xFF 0xB3 0 ; v_srgbi 0 0x80 0 |]
      else
        Array.init A.card ~f:(fun i -> Color.gray (float i *. 127. /. float A.card))
    in
    let draw_letter ~x ~col ~sy l =
      if Core_kernel.Float.(sy < 1e-6) then None
      else
        Some (
          text ~valign:`base ~font l ~col ~x ~y:0.
          |> scale ~center:`origin ~sy
        )
    in
    let draw_col p_i =
      let entropy = column_entropy p_i in
      List.init A.card ~f:(fun j ->
          draw_letter letter.(j) ~col:color.(j) ~x:0. ~sy:p_i.(j)
        )
      |> List.filter_opt
      |> vstack ~align:`centered
      |> reshape ~bbox:(Box2.v (V2.v 0. 0.) (V2.v 1. (max_entropy -. entropy)))
    in
    Array.map t ~f:draw_col
    |> Array.to_list
    |> List.cons (draw_y_scale ())
    |> hstack ~align:`bottom
end

module DNA = struct
  include Make(Nucleotide)

  let reverse_complement mat =
    let open Nucleotide in
    let n = Array.length mat in
    let f i c = mat.(n - i - 1).(to_int c) in
    Array.init n ~f:(fun i ->
        Nucleotide.[| f i t ; f i g ; f i c ; f i a |]
      )
end
