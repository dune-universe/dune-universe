
(*
 * Copyright (c) 2013 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Cow.Html

module Link = struct
  type t = string * Uri.t
  type links = t list
  let link ?(cl="") (txt, uri) = a ~href:uri ~cls:cl (string txt)

  let mk_ul_links ~cl ~links = ul ~cls:cl links

  let top_nav ?(align=`Right) (links:links) =
    let links = List.map (link ~cl:"") links in
    let cl = match align with `Right -> "right" | `Left -> "left" in
    mk_ul_links ~cl ~links

  let button_group (links:links) =
    let links = List.map (link ~cl:"button") links in
    mk_ul_links ~cl:"button-group" ~links

  let side_nav (links:links) =
    let links = List.map (link ~cl:"") links in
    mk_ul_links ~cl:"side-nav" ~links

  let bottom_nav (links:links) =
    let links = List.map (link ~cl:"") links in
    mk_ul_links ~cl:"inline-list right" ~links
end

module Sidebar = struct
  type t = [
    | `link of Link.t
    | `active_link of Link.t
    | `divider
    | `text of string
    | `html of Cow.Xml.t
  ]

  let t ~title ~content =
    let to_html = function
      |`link l        -> li (Link.link l)
      |`active_link l -> li ~cls:"active" (Link.link l)
      |`divider       -> li ~cls:"divider" empty
      |`html h        -> li h
      |`text t        -> li (string t)
    in
    let rec make = function
      | []     -> []
      | hd::tl -> to_html hd :: make tl
    in
    h5 (string title)
    ++ ul ~add_li:false ~cls:"side-nav" (make content)
end

module Index = struct
  let t ~top_nav =
    top_nav
    ++ br
    ++ div ~cls:"row" (
      div ~cls:"large-12 columns" (
        img (Uri.of_string "http://placehold.it/1000x400&amp;text=img")
        ++ hr
      ))
end

let rec intercalate x = function
  | []    -> []
  | [e]   -> [e]
  | e::es -> e :: x :: intercalate x es

module Blog = struct

  let post ~title ~authors ~date ~content =
    let open Link in
    let author = match authors with
      | [] -> empty
      | _  ->
        let a_nodes =
          intercalate (string ", ") (List.map (link ~cl:"") authors)
        in
        string "By " ++ list a_nodes
    in
    let title_text, title_uri = title in
    tag "article" (
      date
      ++ h4 (a ~href:title_uri (string title_text))
      ++ p (i author)
      ++ content
    )

  let t ~title ~subtitle ~sidebar ~posts ~copyright () =
    let subtitle =
      match subtitle with
      | None   -> empty
      | Some s -> small (string s)
    in
    list [
      div ~cls:"row"
        (div ~cls:"large-9 columns" (h2 (string title ++ subtitle)));
      div ~cls:"row" (
        div ~cls:"small-12 large-9 columns" ~attrs:["role", "content"] posts
        ++ aside ~cls:"small-12 large-3 columns panel" sidebar
      );
      footer ~cls:"row" (
        div ~cls:"large-12 columns" (
          hr
          ++ div ~cls:"row" (
            div ~cls:"large-6 columns" (
              p (small (string "&copy; Copyright " ++ copyright))
            ))))
    ]

end

let body ?google_analytics ?highlight ~title:t ~headers ~content ~trailers () =
  (* Cannot be inlined below as the $ is interpreted as an
     antiquotation *)
  let js_init = [`Data "$(document).foundation();"] in
  let highlight_css, highlight_trailer = match highlight with
    | None       -> empty, empty
    | Some style ->
      link ~rel:"stylesheet" (Uri.of_string style),
      script ~src:(Uri.of_string "/js/vendor/highlight.pack.js") empty
      ++ script (string "hljs.initHighlightingOnLoad(); ")
  in
  let ga =
    match google_analytics with
    | None        -> []
    | Some (a, d) ->
      script ~ty:"text/javascript" (
        string @@ Printf.sprintf
          "//<![CDATA[\n\
           var _gaq = _gaq || [];\n\
           _gaq.push(['_setAccount', '%s']);\n\
           _gaq.push(['_setDomainName', '%s']);\n\
           _gaq.push(['_trackPageview']);\n\
           \n\
           (function() {\n\
          \  var ga = document.createElement('script'); \
          \    ga.type = 'text/javascript'; \
          \    ga.async = true;\n\
          \  ga.src = ('https:' == document.location.protocol\
          \    ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';\n\
          \  var s = document.getElementsByTagName('script')[0]; \
          \    s.parentNode.insertBefore(ga, s);\n\
           })();\n\
           //]]>" a d)
  in
  head (list [
      meta ["charset","utf-8"];
      meta ["name","viewport"; "content","width=device-width"];
      title (string t);
      link ~rel:"stylesheet" (Uri.of_string "/css/foundation.min.css");
      link ~rel:"stylesheet" (Uri.of_string "/css/site.css");
      script ~src:(Uri.of_string "/js/vendor/custom.modernizr.js") empty;
      highlight_css;
      ga;
      headers;
    ])
  ++ body (list [
      content;
      script ~src:(Uri.of_string "/js/vendor/jquery.min.js") empty;
      script ~src:(Uri.of_string "/js/foundation/foundation.min.js") empty;
      script ~src:(Uri.of_string "/js/foundation/foundation.topbar.js") empty;
      script js_init;
      highlight_trailer;
      trailers
    ])

let top_nav ~title ~title_uri ~nav_links =
  div ~cls:"contain-to-grid fixed" (
    nav ~cls:"top-bar" ~attrs:["data-topbar",""] (
      ul ~add_li:false ~cls:"title-area" [
        li ~cls:"name" (h1 (a ~href:title_uri title));
        li ~cls:"toggle-topbar menu-icon"
          (a ~href:(Uri.of_string "#") (span (string "Menu")));
      ]
      ++ section ~cls:"top-bar-section" nav_links
    ))

let page ~body =
  Printf.sprintf
    "<!DOCTYPE html>\n\
    \  <!--[if IE 8]><html class=\"no-js lt-ie9\" lang=\"en\" \
     xmlns=\"http://www.w3.org/1999/xhtml\"><![endif]-->\n\
    \  <!--[if gt IE 8]><!--><html class=\"no-js\" lang=\"en\" \
     xmlns=\"http://www.w3.org/1999/xhtml\"><!--<![endif]-->\n\
     %s\n\
     </html>" (Cow.Html.to_string body)
