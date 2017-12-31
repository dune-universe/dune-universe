open Core_kernel

open Int.Replace_polymorphic_compare

type dims = { width : int; height : int }

let sexp_of_dims {width; height} =
  sexp_of_string (sprintf "w%dh%d" width height)

let dims_invariant {width; height} =
  assert (width >= 0);
  assert (height >= 0)

type valign = [`Top | `Bottom | `Center] [@@deriving sexp_of]
type halign = [`Left | `Right | `Center] [@@deriving sexp_of]

type t =
  | Text of string
  | Fill of char * dims
  | Hcat of t * t * dims
  | Vcat of t * t * dims
  | Ansi of string option * t * string option * dims
[@@deriving sexp_of]

let height = function
  | Text _ -> 1
  | Fill (_, d) | Hcat (_, _, d) | Vcat (_, _, d) | Ansi (_, _, _, d) -> d.height

let width = function
  | Text s -> String.length s
  | Fill (_, d) | Hcat (_, _, d) | Vcat (_, _, d) | Ansi (_, _, _, d) -> d.width

let rec invariant t =
  match t with
  | Text s ->
    assert (not (String.mem s '\n'));
  | Fill (_, dims) ->
    dims_invariant dims;
  | Hcat (t1, t2, dims) ->
    dims_invariant dims;
    invariant t1;
    invariant t2;
    [%test_result:int] (height t1) ~expect:dims.height;
    [%test_result:int] (height t2) ~expect:dims.height;
    [%test_result:int] (width t1 + width t2) ~expect:dims.width
  | Vcat (t1, t2, dims) ->
    dims_invariant dims;
    invariant t1;
    invariant t2;
    [%test_result:int] (width t1) ~expect:dims.width;
    [%test_result:int] (width t2) ~expect:dims.width;
    [%test_result:int] (height t1 + height t2) ~expect:dims.height
  | Ansi (_, t, _, dims) ->
    dims_invariant dims;
    invariant t;
    [%test_result:int] (width  t) ~expect:dims.width;
    [%test_result:int] (height t) ~expect:dims.height

let fill_generic ch ~width ~height =
  assert (width >= 0);
  assert (height >= 0);
  Fill (ch, {width; height})

let fill ch ~width ~height = fill_generic ch  ~width ~height
let space   ~width ~height = fill_generic ' ' ~width ~height

let nil = space ~width:0 ~height:0

let hstrut width = space ~width ~height:0
let vstrut height = space ~height ~width:0

let dims t = {width = width t; height = height t}

let halve n =
  let fst = n / 2 in
  let snd = fst + n mod 2 in
  (fst, snd)

let ansi_escape ?prefix ?suffix t = Ansi (prefix, t, suffix, dims t)

let rec hpad t ~align delta =
  assert (delta >= 0);
  if delta = 0 then t else begin
    let height = height t in
    let pad = space ~height ~width:delta in
    match align with
    | `Left   -> Hcat (t, pad, {height; width = width t + delta})
    | `Right  -> Hcat (pad, t, {height; width = width t + delta})
    | `Center ->
      let (delta1, delta2) = halve delta in
      let t = hpad t ~align:`Left  delta1 in
      let t = hpad t ~align:`Right delta2 in
      t
  end

let rec vpad t ~align delta =
  assert (delta >= 0);
  if delta = 0 then t else begin
    let width = width t in
    let pad = space ~width ~height:delta in
    match align with
    | `Top    -> Vcat (t, pad, {width; height = height t + delta})
    | `Bottom -> Vcat (pad, t, {width; height = height t + delta})
    | `Center ->
      let (delta1, delta2) = halve delta in
      let t = vpad t ~align:`Top    delta1 in
      let t = vpad t ~align:`Bottom delta2 in
      t
  end

let max_height ts = List.fold ts ~init:0 ~f:(fun acc t -> Int.max acc (height t))
let max_width  ts = List.fold ts ~init:0 ~f:(fun acc t -> Int.max acc (width  t))

let valign align ts =
  let h = max_height ts in
  List.map ts ~f:(fun t -> vpad ~align t (h - height t))

let halign align ts =
  let w = max_width ts in
  List.map ts ~f:(fun t -> hpad ~align t (w - width t))

let hcat ?(align = `Top) ?sep ts =
  let ts = Option.fold sep ~init:ts ~f:(fun ts sep -> List.intersperse ts ~sep) in
  let ts = valign align ts in
  match ts with
  | [] -> nil
  | t :: ts ->
    List.fold ~init:t ts ~f:(fun acc t ->
      assert (height acc = height t);
      Hcat (acc, t, {height = height acc; width = width acc + width t}))

let vcat ?(align = `Left) ?sep ts =
  let ts = Option.fold sep ~init:ts ~f:(fun ts sep -> List.intersperse ts ~sep) in
  let ts = halign align ts in
  match ts with
  | [] -> nil
  | t :: ts ->
    List.fold ~init:t ts ~f:(fun acc t ->
      assert (width acc = width t);
      Vcat (acc, t, {width = width acc; height = height acc + height t}))

let text_of_lines lines ~align =
  lines
  |> List.map ~f:(fun line -> Text line)
  |> vcat ~align

let text_no_wrap ~align str =
  if String.mem str '\n'
  then String.split ~on:'\n' str |> text_of_lines ~align
  else Text str

let word_wrap str ~max_width =
  String.split str ~on:' '
  |> List.concat_map ~f:(String.split ~on:'\n')
  |> List.fold ~init:(Fqueue.empty, Fqueue.empty, 0)
       ~f:(fun (lines, line, len) word ->
         let n = String.length word in
         let n' = len + 1 + n in
         if n' > max_width
         then (Fqueue.enqueue lines line, Fqueue.singleton word, n)
         else (lines, Fqueue.enqueue line word, n')
       )
  |> (fun (lines, line, _) -> Fqueue.enqueue lines line)
  |> Fqueue.map ~f:(fun line -> Fqueue.to_list line |> String.concat ~sep:" ")
  |> Fqueue.to_list

let text ?(align = `Left) ?max_width str =
  match max_width with
  | None -> text_no_wrap ~align str
  | Some max_width -> word_wrap str ~max_width |> text_of_lines ~align

(* an abstract renderer, instantiated once to compute line lengths and then again to
   actually produce a string. *)
let render_abstract t ~write_direct ~line_length =
  for j = 0 to height t - 1 do write_direct '\n' (line_length j) j done;
  let next_i = Array.init (height t) ~f:(fun _ -> 0) in
  let add_char c j =
    let i = next_i.(j) in
    next_i.(j) <- i + 1;
    write_direct c i j
  in
  let write_string s j =
    for i = 0 to String.length s - 1 do
      add_char s.[i] j
    done
  in
  let rec aux t j_offset =
    match t with
    | Text s -> write_string s j_offset
    | Fill (ch, d) ->
      for _i = 0 to d.width - 1 do
        for j = 0 to d.height - 1 do
          add_char ch (j + j_offset)
        done
      done
    | Vcat (t1, t2, _) ->
      aux t1 j_offset;
      aux t2 (j_offset + height t1)
    | Hcat (t1, t2, _) ->
      aux t1 j_offset;
      aux t2 j_offset;
    | Ansi (prefix, t, suffix, _) ->
      let vcopy s =
        Option.iter s ~f:(fun s ->
          for j = 0 to height t - 1 do
            write_string s (j + j_offset);
          done)
      in
      vcopy prefix;
      aux t j_offset;
      vcopy suffix;
  in
  aux t 0

let line_lengths t =
  let r = Array.create ~len:(height t) 0 in
  let write_direct c i j =
    if not (Char.is_whitespace c) then
      r.(j) <- Int.max r.(j) (i + 1)
  in
  let line_length _ = -1 (* doesn't matter *) in
  render_abstract t ~write_direct ~line_length;
  r

let render t =
  let height = height t in
  if height = 0 then "" else begin
    let line_lengths = line_lengths t in
    let (line_offsets, buflen) =
      let r = Array.create ~len:height 0 in
      let line_offset j = r.(j - 1) + line_lengths.(j - 1) + 1 in
      for j = 1 to height - 1 do r.(j) <- line_offset j done;
      (r, line_offset height)
    in
    let buf = Bytes.make buflen ' ' in
    let write_direct c i j =
      if Char.equal c '\n' || i < line_lengths.(j) then
        Bytes.set buf (i + line_offsets.(j)) c
    in
    let line_length j = line_lengths.(j) in
    render_abstract t ~write_direct ~line_length;
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf
  end

(* header compression *)

let rec cons x = function
  | [] -> [x]
  | y :: zs ->
    if height x < height y then
      x :: y :: zs
    else
      cons (hcat ~align:`Bottom [x; y]) zs

let compress_table_header ?(sep_width = 2) (`Cols cols) =
  let cols =
    List.map cols ~f:(fun (header, data, align) ->
      (header, Int.max 1 (max_width data), halign align data))
  in
  let header =
    hcat ~align:`Bottom begin
      List.fold_right cols ~init:[] ~f:(fun (header, max_width, _) stairs ->
        let rec loop stairs acc =
          let stop () = cons (vcat ~align:`Left [header; acc]) stairs in
          match stairs with
          | [] -> stop ()
          | x :: rest ->
            if width header + sep_width <= width acc then stop () else
              loop rest
                (hcat [
                  vcat ~align:`Left [
                    fill '|' ~width:1 ~height:(height x - height acc);
                    acc;
                  ];
                  x;
                ])
        in
        loop stairs
          (vcat ~align:`Left [
            text "|";
            hstrut (max_width + sep_width);
          ])
      )
    end
  in
  let rows =
    List.map cols ~f:(fun (_, _, data) -> data)
    |> List.transpose_exn
    |> List.map ~f:(fun row -> hcat row ~sep:(hstrut sep_width))
  in
  (`Header header, `Rows rows)

let table ?(sep_width = 2) (`Cols cols) =
  let cols =
    List.map cols ~f:(fun (data, align) ->
      (Int.max 1 (max_width data), halign align data))
  in
  let rows =
    List.map cols ~f:(fun (_, data) -> data)
    |> List.transpose_exn
    |> List.map ~f:(fun row -> hcat row ~sep:(hstrut sep_width))
  in
  `Rows rows

(* convenience definitions *)

let vsep = vstrut 1

let hsep = hstrut 1

let indent ?(n = 2) t = hcat [hstrut n; t]

let sexp sexp_of_a a = sexp_of_a a |> Sexp.to_string_hum |> text

let textf ?align ?max_width fmt =
  ksprintf (text ?align ?max_width) fmt
