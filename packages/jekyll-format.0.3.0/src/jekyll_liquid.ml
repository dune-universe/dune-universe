(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   jekyll-format v0.3.0
  ---------------------------------------------------------------------------*)

open Astring
module JF = Jekyll_format

module Tags = struct
  type highlight = { lang : string option; body : String.Sub.t; linenos : bool }

  let mk_highlight ?lang ?(body = String.Sub.empty) ?(linenos = false) () =
    { lang; body; linenos }

  let pp_highlight ppf { lang; body; linenos } =
    let open Fmt in
    pf ppf "lang: %a linenos: %a@,body:@,%a" (option string) lang String.Sub.pp
      body bool linenos

  let mk_highlight ?lang ?(body = String.Sub.empty) ?(linenos = false) () =
    { lang; body; linenos }

  let highlight tag_data =
    String.cuts ~empty:false ~sep:" " tag_data |> function
    | [ "highlight" ] -> Some (mk_highlight ())
    | [ "highlight"; lang ] -> Some (mk_highlight ~lang ())
    | [ "highlight"; lang; "linenos" ] ->
        Some (mk_highlight ~lang ~linenos:true ())
    | _ -> None

  let endhighlight tag_data = tag_data = "endhighlight"
end

let highlight_markdown_code h s =
  let delim = String.Sub.v "```" in
  String.Sub.concat [ delim; s; delim ]

let highlight_exn ?(f = highlight_markdown_code) body =
  Jekyll_tags.map_liquid_tag_bodies ~f_start:Tags.highlight
    ~f_stop:Tags.endhighlight ~f_map:f body

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
