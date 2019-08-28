open Ctypes
open PosixTypes
open Foreign

(* This is kind of ugly. We brute force try to load every name libcmark might
   be under. Passing the linking flags to the compiler doesn't seem to work
   reliably, so we try to load dynamically here. See
   https://github.com/ocamllabs/ocaml-ctypes/issues/71 .*)
let libnames = [ "libcmark.so"; "libcmark.dylib" ]

let _ =
  let rec loop = function
    | n :: ns -> (
        try
          ignore
          @@ Dl.dlopen ~filename:n ~flags:[ Dl.RTLD_LAZY; Dl.RTLD_GLOBAL ]
        with Dl.DL_error _ -> loop ns )
    | [] -> ()
  in
  loop libnames

type t = { node : unit ptr }

let node_free' = foreign "cmark_node_free" (ptr void @-> returning void)

let finalizer { node } = node_free' node

let finalized (x : t) =
  let () = Gc.finalise finalizer x in
  x

(* Simple interface *)

let markdown_to_html' =
  foreign "cmark_markdown_to_html"
    (string @-> size_t @-> int @-> returning string)

let html_of_commonmark s =
  let sz = Unsigned.Size_t.of_int (String.length s) in
  markdown_to_html' s sz 0

(* Complex interface *)

type render_flag = [ `SourcePos | `HardBreaks | `Safe | `Unsafe ]

type parse_flag = [ `Normalize | `ValidateUTF8 | `Smart ]

let rec int_of_flags = function
  | [] -> 0
  | flag :: rest ->
      let bit =
        match flag with
        | `SourcePos -> 1
        | `HardBreaks -> 2
        | `Safe -> 3
        | `Normalize -> 8
        | `ValidateUTF8 -> 9
        | `Smart -> 10
        | `Unsafe -> 17
      in
      (1 lsl bit) lor int_of_flags rest

let fopen = foreign "fopen" (string @-> string @-> returning (ptr void))

let fclose = foreign "fclose" (ptr void @-> returning int)

(* Parsing *)

let parse_document' =
  foreign "cmark_parse_document"
    (string @-> size_t @-> int @-> returning (ptr void))

let of_string ?(flags : parse_flag list = []) s =
  let sz = Unsigned.Size_t.of_int (String.length s) in
  finalized { node = parse_document' s sz (int_of_flags flags) }

let parse_file' =
  foreign "cmark_parse_file" (ptr void @-> int @-> returning (ptr void))

let of_file ?(flags : parse_flag list = []) path =
  let fh = fopen path "r" in
  if fh = null then `Error "fopen: Failed to open file."
  else
    let node = parse_file' fh (int_of_flags flags) in
    let res = fclose fh in
    if res <> 0 then `Error "fclose: Failed to close file."
    else `Ok (finalized { node })

(* Rendering *)

let render_xml' =
  foreign "cmark_render_xml" (ptr void @-> int @-> returning string)

let render_html' =
  foreign "cmark_render_html" (ptr void @-> int @-> returning string)

let render_man' =
  foreign "cmark_render_man" (ptr void @-> int @-> int @-> returning string)

let render_commonmark' =
  foreign "cmark_render_commonmark"
    (ptr void @-> int @-> int @-> returning string)

let render_latex' =
  foreign "cmark_render_latex" (ptr void @-> int @-> int @-> returning string)

let renderer prim ?(flags : render_flag list = []) { node } =
  prim node (int_of_flags flags)

let rendererw prim ?(flags : render_flag list = []) ~width { node } =
  prim node (int_of_flags flags) width

let to_xml = renderer render_xml'

let to_html = renderer render_html'

let to_man = rendererw render_man'

let to_commonmark = rendererw render_commonmark'

let to_latex = rendererw render_latex'
