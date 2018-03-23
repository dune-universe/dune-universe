open Core_kernel
open Import

let sanitize_sexp s =
  Sexp.to_string s
  |> String.lowercase
  |> String.substr_replace_all ~pattern:"_" ~with_:"-"

type css_global_values =
  [ `Inherit | `Initial ]
[@@deriving sexp, bin_io, compare]

module Color = struct
  module T = struct
    module RGBA = struct
      type t =
        { r : int
        ; g : int
        ; b : int
        ; a : Percent.t option
        } [@@deriving sexp, bin_io, compare, fields]

      let create ~r ~g ~b ?a () = { r; g; b; a }
    end

    type simple =
      [ `Black
      | `Blue
      | `Brown
      | `Yellow
      | `Cyan
      | `Green
      | `Grey
      | `Magenta
      | `Purple
      | `Red
      | `White
      | `SteelBlue
      | `Honeydew
      | `Whitesmoke
      | `LawnGreen
      | `GoldenRod
      | `DarkGreen
      | `DarkMagenta
      | `DarkRed
      ] [@@deriving sexp, bin_io, compare]

    type t =
      [ simple | `RGBA of RGBA.t | css_global_values ]
    [@@deriving sexp, bin_io, compare]
  end
  include T
  include Sexpable.To_stringable (T)

  let to_string_css = function
    | #simple
    | #css_global_values as c -> sexp_of_t c |> sanitize_sexp
    | `RGBA { RGBA. r; g; b; a} ->
      match a with
      | None -> sprintf "rgb(%i,%i,%i)" r g b
      | Some p -> sprintf "rgba(%i,%i,%i,%.2f)" r g b (Percent.to_mult p)
end

module Alignment = struct
  type t =
    [ `Left | `Right  | `Center  (* horizontal *)
    | `Top  | `Bottom | `Middle  (* vertical *)
    | `Justify  (* text-align (in addition to [horizontal]) *)
    | css_global_values
    ]
  [@@deriving sexp, bin_io, compare]
  include Sexpable.To_stringable (struct type nonrec t = t [@@deriving sexp] end)

  let to_string_css = Fn.compose sanitize_sexp sexp_of_t
end

module Length = struct
  type t =
    [ `Px of int
    | `Em of int
    | `Vh of Percent.t
    | `Vw of Percent.t
    | `Percent of Percent.t
    | css_global_values
    ]
  [@@deriving sexp, bin_io, compare]

  let to_string_css = function
    | `Px i -> sprintf "%ipx" i
    | `Em i -> sprintf "%iem" i
    | `Vh p -> sprintf "%.2fvh" (Percent.to_percentage p)
    | `Vw p -> sprintf "%.2fvw" (Percent.to_percentage p)
    | `Percent p -> sprintf "%.2f%%" (Percent.to_percentage p)
    | #css_global_values as l ->
      sexp_of_css_global_values l |> sanitize_sexp
end

module Auto_or_length = struct
  type t =
    [ `Auto
    | Length.t
    ]
  [@@deriving bin_io, compare, sexp]

  let to_string_css = function
    | `Auto -> "auto"
    | #Length.t as l -> Length.to_string_css l
end

let value_map o ~f = Option.value_map o ~default:"" ~f

type t = (string * string) list [@@deriving sexp, compare]

let combine t1 t2 = t1 @ t2
let ( @> ) = combine
let concat l = List.concat l

let to_attr = Vdom.Attr.style
let to_string_css t =
  List.map t ~f:(fun (field, value) -> sprintf "%s: %s" field value)
  |> String.concat ~sep:";"

let create ~field ~value = [(field, value)]
let empty = []

let box_sizing v =
  let value =
    [%sexp_of:[ `Content_box | `Border_box | css_global_values ]] v
    |> sanitize_sexp
  in
  create ~field:"box-sizing" ~value

let display v =
  let value =
    [%sexp_of:
      [ `Inline | `Block | `Inline_block | `List_item | `Table | `Inline_table | `None
      (* [ `Flex | `Inline_flex ] are intentionally left out of the mli to force the user
         to use [flex_container] below *)
      | `Flex | `Inline_flex
      | css_global_values ]]
      v
    |> sanitize_sexp
  in
  create ~field:"display" ~value

let visibility v =
  let value =
    [%sexp_of:[`Visible | `Hidden | `Collapse | css_global_values ]] v
    |> sanitize_sexp
  in
  create ~field:"visibility" ~value

let overflow v =
  let value =
    [%sexp_of:[ `Visible | `Hidden | `Scroll | `Auto | css_global_values ]] v
    |> sanitize_sexp
  in
  create ~field:"overflow" ~value


let z_index i = create ~field:"z-index" ~value:(Int.to_string i)
let opacity i = create ~field:"opacity" ~value:(Int.to_string i)

let create_length_field field =
  fun l -> create ~field ~value:(Auto_or_length.to_string_css l)

type font_style = [ `Normal | `Italic | `Oblique | css_global_values ]
type font_weight = [ `Normal | `Bold | `Bolder | `Lighter | `Number of int | css_global_values ]
type font_variant = [ `Normal | `Small_caps | css_global_values ]

let font_size = create_length_field "font-size"

let font_family l = create ~field:"font-family" ~value:(String.concat l ~sep:",")

let font_style s =
  let value =
    [%sexp_of: [ `Normal | `Italic | `Oblique | css_global_values ]] s
    |> sanitize_sexp
  in
  create ~field:"font-style" ~value

let font_weight =
  let module Static_weight = struct
    type t = [ `Normal | `Bold | `Bolder | `Lighter | css_global_values ]
    [@@deriving sexp]
  end in
  fun s ->
    let value =
      match s with
      | `Number i -> Int.to_string i
      | #Static_weight.t as x ->
        Static_weight.sexp_of_t x
        |> sanitize_sexp
    in
    create ~field:"font-weight" ~value

let bold = font_weight `Bold

let font_variant s =
  let value =
    [%sexp_of: [ `Normal | `Small_caps | css_global_values ]] s
    |> sanitize_sexp
  in
  create ~field:"font-variant" ~value

let font ~size ~family ?style ?weight ?variant () =
  let m = Option.map in
  font_size size
  @> font_family family
  @> ([ m style   ~f:font_style
      ; m weight  ~f:font_weight
      ; m variant ~f:font_variant
      ] |> List.filter_opt |> concat)

let color c = create ~field:"color" ~value:(Color.to_string_css c)
let background_color c = create ~field:"background-color" ~value:(Color.to_string_css c)

let create_alignment field =
  fun a ->
    create ~field ~value:(Alignment.to_string_css (a :> Alignment.t))

let text_align       = create_alignment "text-align"
let horizontal_align = create_alignment "horizontal-align"
let vertical_align   = create_alignment "vertical-align"

let width     = create_length_field "width"
let min_width = create_length_field "min-width"
let max_width = create_length_field "max-width"

let height     = create_length_field "height"
let min_height = create_length_field "min-height"
let max_height = create_length_field "max-height"

let padding_top    = create_length_field "padding-top"
let padding_bottom = create_length_field "padding-bottom"
let padding_left   = create_length_field "padding-left"
let padding_right  = create_length_field "padding-right"
let padding ?top ?bottom ?left ?right () =
  let m = Option.map in
  [ m top    ~f:padding_top
  ; m bottom ~f:padding_bottom
  ; m left   ~f:padding_left
  ; m right  ~f:padding_right
  ] |> List.filter_opt |> concat

let margin_top    = create_length_field "margin-top"
let margin_bottom = create_length_field "margin-bottom"
let margin_left   = create_length_field "margin-left"
let margin_right  = create_length_field "margin-right"
let margin ?top ?bottom ?left ?right () =
  let m = Option.map in
  [ m top    ~f:margin_top
  ; m bottom ~f:margin_bottom
  ; m left   ~f:margin_left
  ; m right  ~f:margin_right
  ] |> List.filter_opt |> concat

type border_style =
  [ `None | `Hidden | `Dotted | `Dashed | `Solid
  | `Double | `Groove | `Ridge | `Inset | `Outset
  | css_global_values ]

let create_border ?side () =
  let side =
    value_map side ~f:(fun side ->
      side
      |> [%sexp_of:[`Top | `Right | `Bottom | `Left ]]
      |> sanitize_sexp
    )
  in
  fun ?width ?color ~style () ->
    let style =
      [%sexp_of:[ `None | `Hidden | `Dotted | `Dashed | `Solid
                | `Double | `Groove | `Ridge | `Inset | `Outset
                | css_global_values ]]
        style
      |> sanitize_sexp
    in
    let width = value_map width ~f:Length.to_string_css in
    let color = value_map color ~f:Color.to_string_css in
    create ~field:("border-" ^ side) ~value:(sprintf "%s %s %s" width style color)

let border_top    = create_border ~side:`Top    ()
let border_bottom = create_border ~side:`Bottom ()
let border_left   = create_border ~side:`Left   ()
let border_right  = create_border ~side:`Right  ()
let border        = create_border               ()

let border_collapse v =
  let value =
    [%sexp_of:[`Separate | `Collapse | css_global_values]] v
    |> sanitize_sexp
  in
  create ~field:"border-collapse" ~value

let border_spacing = create_length_field "border-spacing"

let flex_container ?(inline=false) ?(direction=`Row) ?(wrap=`Nowrap) () =
  let direction =
    [%sexp_of:[ `Row | `Row_reverse | `Column | `Column_reverse ]] direction
    |> sanitize_sexp
  in
  let wrap =
    [%sexp_of:[ `Nowrap | `Wrap | `Wrap_reverse ]] wrap
    |> sanitize_sexp
  in
  concat
    [ display (if inline then `Inline_flex else `Flex)
    ; create ~field:"flex-direction" ~value:direction
    ; create ~field:"flex-wrap" ~value:wrap
    ]

let flex_item ?order ?(basis=`Auto) ?(shrink=1.) ~grow () =
  let order =
    Option.map order ~f:(fun i -> create ~field:"order" ~value:(Int.to_string i))
    |> Option.to_list
    |> List.join
  in
  let flex =
    let basis = Auto_or_length.to_string_css basis in
    create ~field:"flex" ~value:(sprintf "%f %f %s" grow shrink basis)
  in
  concat
    [ flex
    ; order
    ]

let animation
      ~name ~duration
      ?delay ?direction ?fill_mode
      ?iter_count ?timing_function
      ()
  =
  let m = Option.map in
  let span_to_string s = sprintf "%.2fs" (Time_ns.Span.to_sec s) in
  let direction =
    m direction ~f:(fun d ->
      let value =
        d
        |> [%sexp_of:[ `Normal | `Reverse | `Alternate | `Alternate_reverse | css_global_values ]]
        |> sanitize_sexp
      in
      create ~field:"animation-direction" ~value)
  in
  let fill_mode =
    m fill_mode ~f:(fun f ->
      let value =
        [%sexp_of:[ `None | `Forwards | `Backwards | `Both | css_global_values ]] f
        |> sanitize_sexp
      in
      create ~field:"animation-fill-mode" ~value)
  in
  [ Some (create ~field:"animation-name" ~value:name)
  ; Some (create ~field:"animation-duration" ~value:(span_to_string duration))
  ; m delay ~f:(fun s -> create ~field:"animation-delay" ~value:(span_to_string s))
  ; m iter_count ~f:(fun i -> create ~field:"animation-iteration-count" ~value:(Int.to_string i))
  ; m timing_function ~f:(fun value -> create ~field:"animation-timing-function" ~value)
  ; direction
  ; fill_mode
  ]
  |> List.filter_opt
  |> concat

