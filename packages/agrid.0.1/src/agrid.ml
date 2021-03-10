(** {1 Introduction} *)

(** Adjustable grids are two dimensional arrays whose width/height can be changed by adding
    or removing row/column at either end (one at a time).

    Implemented using the {{:https://github.com/backtracking/flex-array}
    flex-array library by Jean-Christophe Filliâtre}. *)

(** The type of grids. This is an immutable data structure. Values of type
    ['a t] can be compared using structural equality [=] (provided the elements
    of type ['a] can). *)
type +'a t = 'a Flex_array.t Flex_array.t

(** {1 Using grids} *)

(** [width g] returns the width of [g], that is to say its number of columns. *)
let width (grid : 'a t) =
  if Flex_array.length grid = 0 then
    0
  else
    Flex_array.length (Flex_array.get grid 0)

(** [height g] returns the height of [g], that is to say its number of rows. *)
let height (grid : 'a t) = Flex_array.length grid

(** [dim g] returns a pair in which the first element is the width of [g] and
    the second one its height. *)
let dim (grid : 'a t) =
  let height = Flex_array.length grid in
  if height = 0 then
    (0, 0)
  else
    (Flex_array.length (Flex_array.get grid 0), height)

(** [get g ~x ~y] returns the element at width-index [~x] and height-index [~y]
    in grid [g]. Index start at 0. *)
let get (grid : 'a t) ~x ~y =
  let row = Flex_array.get grid y in
  Flex_array.get row x

(** [get_row g y] returns the row at height-index [y] in grid [g]. Index start
    at 0. *)
let get_row (grid : 'a t) y = Flex_array.get grid y

(** [get_col g x] returns the column at width-index [x] in grid [g]. Index start
    at 0. *)
let get_col (grid : 'a t) y =
  Flex_array.map (fun row -> Flex_array.get row y) grid

(** {1 Building grids} *)

(** The empty grid. *)
let empty : 'a t = Flex_array.empty

(** [of_array a] returns a grid from the two dimensional array [a] with the same
    height, width and order. *)
let of_array a : 'a t = Flex_array.of_array (Array.map Flex_array.of_array a)

(** [of_list l] returns a grid from the two dimensional list [l] with the same
    height, width and order. *)
let of_list l : 'a t = Flex_array.of_list (List.map Flex_array.of_list l)

(** [set g ~x ~y v] returns a new grid where all elements are identical to those
    of [g], except at width-index [x] and height-index [y] where the element is
    [v]. *)
let set (grid : 'a t) ~x ~y v : 'a t =
  let row = Flex_array.get grid y in
  let row = Flex_array.set row x v in
  Flex_array.set grid y row

(** [cons_row row grid] returns a new grid obtained by appending the row [row]
    at the height-axis front of grid [g]. *)
let cons_row row (grid : 'a t) : 'a t = Flex_array.cons row grid

(** [snow_row grid row] returns a new grid obtained by appending the row [row]
    at the height-axis end of grid [g]. *)
let snoc_row (grid : 'a t) row : 'a t = Flex_array.snoc grid row

(** [cons_col col grid] returns a new grid obtained by appending the column
    [col] at the width-axis front of grid [g]. *)
let cons_col col (grid : 'a t) : 'a t =
  if grid = empty then
    Flex_array.map (fun el -> Flex_array.cons el Flex_array.empty) col
  else
    Flex_array.mapi
      (fun i row -> Flex_array.cons (Flex_array.get col i) row)
      grid

(** [snow_col grid col] returns a new grid obtained by appending the column
    [col] at the width-axis end of grid [g]. *)
let snoc_col (grid : 'a t) col : 'a t =
  if grid = empty then
    Flex_array.map (fun el -> Flex_array.cons el Flex_array.empty) col
  else
    Flex_array.mapi
      (fun i row -> Flex_array.snoc row (Flex_array.get col i))
      grid

(** [tail_row g] returns a new grid obtained by removing the row at the
    height-axis front of grid [g]. *)
let tail_row (grid : 'a t) : 'a t = Flex_array.tail grid

(** [liat_row g] returns a new grid obtained by removing the row at the
    height-axis end of grid [g]. *)
let liat_row (grid : 'a t) : 'a t = Flex_array.liat grid

(** [tail_col g] returns a new grid obtained by removing the column at the
    width-axis front of grid [g]. *)
let tail_col (grid : 'a t) : 'a t = Flex_array.map Flex_array.tail grid

(** [liat_col g] returns a new grid obtained by removing the column at the
    width-axis end of grid [g]. *)
let liat_col (grid : 'a t) : 'a t = Flex_array.map Flex_array.liat grid

(** {1 Iterators} *)

(** [iter f g] applies function [f] in turn to all the elements of [g]. It goes
    row by row. It is equivalent to
    [f (get g ~x:0 ~y:0); f (get g ~x:1 ~y:0); ...; f (get g ~x:(w - 1) ~y:0);
    ...; f (get g ~x:0 ~y:(h - 1)); f (get g ~x:1 ~y:(h -1)); ...; f (get g
    ~x:(w - 1) ~y:(h - 1))] where [w] and [h] are respectively the width and the
    height of [g] but runs faster. *)
let iter f (grid : 'a t) = Flex_array.iter (Flex_array.iter f) grid

(** Same as {!iter}, but the function is applied with the width-index and
    height-index of the element as first and second argument, and the element
    itself as third argument. *)
let iteri f (grid : 'a t) =
  Flex_array.iteri
    (fun y row -> Flex_array.iteri (fun x v -> f ~x ~y v) row)
    grid

(** [map f g] returns a grid with the same dimension than [g] but where each
    element [x] has been replaced by the value of [f x]. *)
let map f (grid : 'a t) : 'b t = Flex_array.map (Flex_array.map f) grid

(** Same as {!map}, but the function is applied with the width-index and
    height-index of the element as first and second arguement, and the element
    itself as third argument. *)
let mapi f (grid : 'a t) : 'b t =
  Flex_array.mapi (fun x row -> Flex_array.mapi (fun y v -> f ~x ~y v) row) grid

(** [fold f acc g] computes [Flex_array.fold (Flex_array.fold f) acc g]. *)
let fold f acc (grid : 'a t) = Flex_array.fold (Flex_array.fold f) acc grid

(** [pp ?pp_sep_row ?pp_sep_col pp_v fmt g] prints items of grid [g] using
    [pp_v] to print each item, calling [pp_sep_row] between rows and
    [pp_sep_col] between items (i.e. between each column). [pp_sep_row] defaults
    to {!Format.pp_print_cut} and [pp_print_col] defaults to
    [fun fmt () -> Format.fprintf fmt ";"]. Does nothing on empty grids. *)
let pp ?(pp_sep_row = Format.pp_print_newline)
    ?(pp_sep_col = fun fmt () -> Format.fprintf fmt ";") pp_v fmt (grid : 'a t)
    =
  Flex_array.pp ~pp_sep:pp_sep_row
    (fun fmt el -> Flex_array.pp ~pp_sep:pp_sep_col pp_v fmt el)
    fmt grid
