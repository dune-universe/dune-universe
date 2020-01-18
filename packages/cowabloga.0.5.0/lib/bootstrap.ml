(*
 * Copyright (c) 2014 Richard Mortier <mort@cantab.net>
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

let body ?google_analytics ~title:t ~headers content =
  let ga = match google_analytics with
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
          \  var ga = document.createElement('script');\n\
          \  ga.type = 'text/javascript'; ga.async = true;\n\
          \  ga.src =\n\
          \    ('https:' == document.location.protocol\n\
          \    ? 'https://ssl'\n\
          \    : 'http://www') + '.google-analytics.com/ga.js';\n\
          \  var s = document.getElementsByTagName('script')[0];\n\
          \  s.parentNode.insertBefore(ga, s);\n\
           })();'\n\
           //]]>" a d
      ) in
  let uri = Uri.of_string in
  head (list [
      meta ["charset","utf-8"];
      meta [
        "http-equiv","Content-Type";
        "content"   ,"text/html";
        "charset"   ,"UTF-8"
      ];
      meta [
        "name"         , "viewport";
        "content"      ,"width=device-width";
        "initial-scale","1";
        "maximum-scale","1"
      ];
      string (
        " <!-- Le HTML5 shim, for IE6-8 support of HTML elements -->\n\
         <!--[if lt IE 9]>");
      script
        ~src:(uri "http://html5shim.googlecode.com/svn/trunk/html5.js") empty;
      string ("<![endif]-->");

      title (string t);

      link ~rel:"stylesheet" ~media:"screen" ~ty:"text/css"
        (uri "/css/bootstrap.min.css");
      link ~rel:"stylesheet" ~media:"screen" ~ty:"text/css"
        (uri "/css/bootstrap-responsive.min.css");
      link ~rel:"stylesheet" (uri "/css/site.css");
      script ~src:(uri "/js/jquery-1.9.1.min.js") empty;
      script ~src:(uri "/js/bootstrap.min.js") empty;

      headers
    ])
  ++ body (div ~cls:"container-fluid" content ++ ga)

let page ?(ns="") body =
  Printf.sprintf
    "<!DOCTYPE html>\n\
     <html '%s'>\n\
     %s\n\
     </html>" ns (Cow.Html.to_string body)
