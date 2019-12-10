(* TODO: profile and optimize performance, currently we only get ~250k updates per second
   on a terminal width of ~120 on an i7-8565U.
*)
open Base

module Style = struct
  type t =
    | Utf
    | Ascii
    | Line
    | Circle
    | Braille
    | Braille_spin
    | Vertical

  let bars = function
    | Utf -> [| " "; "▏"; "▎"; "▍"; "▌"; "▋"; "▊"; "▉"; "█" |]
    | Ascii -> [| " "; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "#" |]
    | Line -> [| "─"; "─"; "─"; "╾"; "╾"; "╾"; "╾"; "━"; "═" |]
    | Circle -> [| " "; "◓"; "◑"; "◒"; "◐"; "◓"; "◑"; "◒"; "#" |]
    | Braille -> [| " "; "⡀"; "⡄"; "⡆"; "⡇"; "⡏"; "⡟"; "⡿"; "⣿" |]
    | Braille_spin -> [| " "; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠇"; "⠿" |]
    | Vertical -> [| "▁"; "▂"; "▃"; "▄"; "▅"; "▆"; "▇"; "█"; "█" |]
end

module Options = struct
  type t =
    { style : Style.t
    ; total_width : int option
    ; prefix : string
    }

  let default = { style = Utf; total_width = None; prefix = "" }
end

type t =
  { options : Options.t
  ; total_width : int
  ; buffer : Buffer.t
  ; total : int
  ; bars : string array
  ; out_channel : Stdio.Out_channel.t
  ; isatty : bool
  ; mutable current : int
  ; mutable start_time : Utils.Time.t
  }

let create ?(options = Options.default) total =
  let total_width =
    match options.total_width with
    | Some total_width -> total_width
    | None -> Term_width.get () |> Option.value ~default:0
  in
  { options
  ; total_width
  ; start_time = Utils.Time.now ()
  ; buffer = Buffer.create (total_width + 1)
  ; total
  ; current = 0
  ; bars = Style.bars options.style
  ; out_channel = Stdio.stdout
  ; isatty = Unix.isatty Unix.stdout
  }

let right_bar ~current ~total ~elapsed ~remaining ~rate =
  Printf.sprintf
    "| %d/%d [%s<%s, %s]"
    current
    total
    (Utils.Time.Span.format elapsed)
    (Utils.Time.Span.format remaining)
    (Utils.format_rate rate)

let left_bar ~current ~total =
  let pct = Float.of_int current /. Float.of_int total *. 100. in
  Printf.sprintf "\r%3.0f%%|" pct

let fill buffer ~options ~current ~total ~bars ~width ~elapsed:_ =
  let current_f = Float.of_int current in
  let total_f = Float.of_int total in
  let bar_len = Array.length bars in
  let fills = current_f /. total_f *. Float.of_int width in
  let ifills = Int.of_float fills in
  Buffer.add_string buffer options.Options.prefix;
  for _i = 1 to ifills do
    Buffer.add_string buffer bars.(bar_len - 1)
  done;
  if current <> total
  then (
    let i = Float.of_int bar_len *. (fills -. Float.of_int ifills) in
    Buffer.add_string buffer bars.(Int.of_float i));
  for _i = 1 to width - ifills - 1 do
    Buffer.add_string buffer bars.(0)
  done

let update t v =
  let v = Int.max 0 (Int.min v t.total) in
  t.current <- v;
  if t.isatty
  then (
    let elapsed = Utils.Time.(diff (now ()) t.start_time) in
    (* TODO: add EMA ? *)
    let rate = Float.of_int t.current /. Utils.Time.Span.to_secs elapsed in
    let remaining =
      Float.of_int (t.total - t.current) /. rate |> Utils.Time.Span.of_secs
    in
    let left_bar = left_bar ~current:t.current ~total:t.total in
    let right_bar =
      right_bar ~current:t.current ~total:t.total ~elapsed ~remaining ~rate
    in
    let width = t.total_width - String.length left_bar - String.length right_bar in
    Buffer.reset t.buffer;
    if width > 0
    then
      fill
        t.buffer
        ~options:t.options
        ~current:t.current
        ~total:t.total
        ~width
        ~bars:t.bars
        ~elapsed;
    let bar = left_bar ^ Buffer.contents t.buffer ^ right_bar in
    Stdio.Out_channel.output_string t.out_channel bar;
    Stdio.Out_channel.flush t.out_channel)

let close t =
  Stdio.Out_channel.output_char t.out_channel '\n';
  Stdio.Out_channel.flush t.out_channel

let reset t =
  t.start_time <- Utils.Time.now ();
  update t 0

let incr t ~by = update t (t.current + by)

let with_bar ?options total ~f =
  let t = create ?options total in
  Exn.protectx ~f t ~finally:close
