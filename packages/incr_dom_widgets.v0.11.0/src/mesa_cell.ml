open Core_kernel
open Import


module Id = struct
  module T = struct
    type t =
      { column_index : int
      ; cell_index   : int
      } [@@deriving bin_io, compare, hash, sexp, fields]
    include Sexpable.To_stringable (struct type nonrec t = t [@@deriving sexp] end)
    let module_name = "Cell_id"
    let create = Fields.create

    let compare =
      let cmp ~field t1 t2 =
        Int.compare
          (Field.get field t1)
          (Field.get field t2)
      in
      Comparable.lexicographic
        [ cmp ~field:Fields.column_index
        ; cmp ~field:Fields.cell_index
        ]
  end
  include T
  include Identifiable.Make(T)
end

module Mode = struct
  type t =
    | View
    | Edit of string Id.Map.t
  [@@deriving compare]
end

module Kind = struct
  type 'a t =
    { name : string
    ; of_string : (string -> 'a)
    ; to_string : ('a -> string)
    } [@@deriving fields]

  let internal_create name of_string to_string =
    { name; of_string; to_string }

  let of_string_safe t =
    fun a ->
      Or_error.try_with (fun () -> t.of_string a)
      |> Result.map_error ~f:(Error.tag ~tag:t.name)

  let of_abbrev_int_string s ~of_int ~fallback =
    let base, factor' =
      match String.lfindi s ~f:(fun _ -> Char.is_alpha) with
      | None -> s, None
      | Some i -> String.prefix s i, Some (String.drop_prefix s i)
    in
    let factor mult =
      Int.of_string base * mult
      |> of_int
    in
    match factor' with
    | None     -> fallback s
    | Some "h" -> factor 100
    | Some "k" -> factor 1_000
    | Some "m" -> factor 1_000_000
    | Some _   -> failwithf "improper compact-int format: %s" s ()

  let float_of_abbrev_int_string =
    of_abbrev_int_string ~of_int:Float.of_int ~fallback:Float.of_string

  let int_of_abbrev_int_string =
    of_abbrev_int_string ~of_int:Fn.id ~fallback:Int.of_string

  let string = internal_create "string" Fn.id Fn.id

  let bool =
    internal_create "bool"
      (function
        | "T" | "t" -> true
        | "F" | "f" -> false
        | s -> Bool.of_string (String.lowercase s))
      (fun b -> if b then "T" else "F")

  let int ?delimiter () =
    internal_create "int" int_of_abbrev_int_string (Int.to_string_hum ?delimiter)

  let float ?decimals ?delimiter ?strip_zero () =
    internal_create "float" float_of_abbrev_int_string
      (Float.to_string_hum ?decimals ?delimiter ?strip_zero)

  let dollar =
    let to_string d =
      sprintf "$%s" (Float.to_string_hum ~decimals:2 ~delimiter:',' ~strip_zero:false d)
    in
    internal_create "dollar" float_of_abbrev_int_string to_string

  let percent = internal_create "percent" Percent.of_string Percent.to_string

  let compact_num ?prepend ?append name of_string to_float =
    let to_string =
      let to_string t = Float.to_padded_compact_string (to_float t) in
      match prepend, append with
      | None  , None   -> to_string
      | Some p, None   -> fun x -> sprintf "%s%s"   p (to_string x)
      | None  , Some a -> fun x -> sprintf "%s%s"     (to_string x) a
      | Some p, Some a -> fun x -> sprintf "%s%s%s" p (to_string x) a
    in
    internal_create name of_string to_string

  let compact_float = compact_num "compact_float" float_of_abbrev_int_string Fn.id
  let compact_int = compact_num "compact_int" int_of_abbrev_int_string Float.of_int
  let compact_dollar =
    compact_num "compact_dollar" ~prepend:"$" float_of_abbrev_int_string Fn.id

  let compact_num ?prepend ?append ~of_string ~to_float () =
    compact_num ?prepend ?append "compact_num" of_string to_float

  let span =
    (* See comment in ../protocol/rpc_protocol.ml  *)
    let to_string time_ns_span =
      Time_ns.Span.Alternate_sexp.sexp_of_t time_ns_span
      |> Sexp.to_string
    in
    let of_string string =
      Sexp.of_string string
      |> Time_ns.Span.Alternate_sexp.t_of_sexp
    in
    internal_create "time_span" of_string to_string

  let option ?(none_string="") t =
    let of_string s =
      match s with
      | "(None)" | "()" | "" -> None
      | _ -> Some (of_string t s)
    in
    let to_string a =
      Option.value_map a ~default:none_string ~f:(to_string t)
    in
    internal_create (name t ^ " option") of_string to_string

  let or_error t =
    let of_string s =
      try Ok (of_string t s)
      with _ ->
        Sexp.of_string s
        |> Or_error.t_of_sexp (Fn.compose (of_string t) Sexp.to_string)
    in
    let to_string = function
      | Ok a -> to_string t a
      | Error e -> Error.tag ~tag:"Error" e |> Error.to_string_mach
    in
    internal_create (name t ^ " or_error") of_string to_string

  let user_defined = Fields.create
end

type ('row, 'a) t =
  { kind  : 'a Kind.t
  ; read  : 'row -> 'a
  ; write : ('row -> 'a -> 'row) option
  ; style : 'a -> Css.t
  } [@@deriving fields]

module Packed = struct
  type ('row, 'a) cell = ('row, 'a) t
  type 'row t =
    | T : ('row, 'a) cell -> 'row t
end

let pack t = Packed.T t

let is_editable (Packed.T t) = Option.is_some t.write

let render_edit id html_id style ~placeholder ~of_string ~current_edit ~remember_edit =
  let open Vdom in
  let is_valid =
    Option.value_map current_edit ~default:true ~f:(fun v ->
      if String.is_empty v
      then true
      else (Result.is_ok (of_string v)))
  in
  let class_ =
    if is_valid
    then "valid-edit"
    else "invalid-edit"
  in
  let style =
    [ Css.background_color `Inherit
    ; Css.color `Inherit
    ; Css.width (`Percent (Percent.of_mult 1.))
    ; Css.box_sizing `Border_box
    ; Css.border ~style:`None ()
    ; Css.font_size (`Em 1)
    ; Css.text_align `Inherit
    ; style
    ] |> Css.concat
  in
  let tab_index =
    Vdom.Attr.create
      "tabindex"
      (sprintf "%i%03i" (Id.column_index id) (Id.cell_index id))
  in
  Node.input
    [ Attr.id html_id
    ; Attr.class_ class_
    ; Attr.placeholder placeholder
    ; Attr.create "type" "text"
    ; Attr.on_input (fun _ value -> remember_edit id value)
    ; Css.to_attr style
    ; tab_index
    ]
    []

let render_view html_id style text =
  let open Vdom in
  Node.div
    [ Attr.id html_id
    ; Css.to_attr style
    ]
    [ Node.text text ]

let create kind ~read ?write ?(style=Fn.const Css.empty) () =
  { kind
  ; read
  ; write
  ; style
  }

let view packed row id mode ~html_id ~remember_edit =
  let Packed.T cell = packed in
  let value = cell.read row in
  let style =
    [ Css.padding_left  (`Px 10)
    ; Css.padding_right (`Px 10)
    ; cell.style value
    ] |> Css.concat
  in
  match (mode:Mode.t), cell.write with
  | View, _
  | _, None ->
    render_view html_id style (Kind.to_string cell.kind value)
  | Edit current_edits, Some _inject ->
    render_edit
      id html_id style
      ~placeholder:(Kind.to_string cell.kind value)
      ~of_string:(Kind.of_string_safe cell.kind)
      ~current_edit:(Map.find current_edits id)
      ~remember_edit

let apply_edit (packed : 'row Packed.t) (row : 'row) (value : string) =
  let Packed.T t = packed in
  match write t with
  | None -> Ok row
  | Some inject ->
    match Kind.of_string_safe t.kind value with
    | Error error ->
      Or_error.error_s [%message
        "Could not parse value"
          (value)
          (error : Error.t)]
    | Ok typed_value -> Ok (inject row typed_value)

let status ~num_active ~num_inactive ~num_not_synchronized ~num_dead =
  let pad =
    Css.concat
      [ Css.padding_left (`Px 0)
      ; Css.padding_right (`Px 0)
      ]
  in
  let group prepend_string color' ~f =
    [ create
        Kind.string
        ~read:(fun m -> if f m = 0 then "" else prepend_string)
        ~style:(const (Css.(concat [ color color'; pad ])))
        ()
      |> pack
    ; create
        Kind.string
        ~read:(fun m -> let i = f m in if i = 0 then "" else (Int.to_string i))
        ~style:(const pad)
        ()
      |> pack
    ]
  in
  group   "#" `Green   ~f:num_active
  @ group "!" `Red     ~f:num_inactive
  @ group "?" `Yellow  ~f:num_not_synchronized
  @ group "X" `Magenta ~f:num_dead
  @ [ create
        Kind.string
        ~read:(fun m ->
          if List.for_all ~f:(fun f -> f m = 0)
               [ num_active; num_inactive; num_not_synchronized; num_dead ]
          then "NOROUTES"
          else "")
        ~style:(Css.(concat [ color `Cyan; bold; pad ]) |> const)
        ()
      |> pack
    ]

module Style = struct
  type 'a t = 'a -> Css.t

  let concat l = fun a ->
    List.map l ~f:(fun f -> f a) |> Css.concat

  let bool =
    let centered =
      Css.(concat
             [ text_align `Center
             ; horizontal_align `Center
             ])
    in
    fun bool ->
      if bool
      then Css.(concat [ centered; color `Green ])
      else Css.(concat [ centered; color `Red ])

  type theme = [ `Light | `Dark ]

  let compact_num ~theme to_float =
    let aligned =
      Css.(concat
             [ text_align `Right
             ; horizontal_align `Right
             ])
    in
    let themed_color =
      match theme with
      | `Dark ->
        fun v ->
          if      v < 999.95E0 then `White
          else if v < 999.95E3 then `Yellow
          else if v < 999.95E6 then `Green
          else if v < 999.95E9 then `Magenta
          else `Red
      | `Light ->
        fun v ->
          if      v < 999.95E0 then `Black
          else if v < 999.95E3 then `GoldenRod
          else if v < 999.95E6 then `DarkGreen
          else if v < 999.95E9 then `DarkMagenta
          else `DarkRed
    in
    fun v ->
      let v = Float.abs (to_float v) in
      let color' = themed_color v in
      Css.(concat [ aligned; color color' ])

  let compact_float ~theme = compact_num ~theme Fn.id
  let compact_int   ~theme = compact_num ~theme Float.of_int
end
