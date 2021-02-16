{

  let togglescript = 
    "<script type=\"text/javascript\" src=\"prototype.js\"></script>\n\
<script type=\"text/javascript\">\n\
<!--\n\
    function toggle_visibility(id) {$(id).toggle(); }\n\
    function toggle_css_ident(event,toggled_class) \n\
      {$$(toggled_class).invoke('toggle');\n\
       var elt = event.findElement();\n\
       if(elt.textContent.startsWith('\\nhide')){\n\
         elt.textContent = elt.textContent.sub('hide','show');\n\
       }else{\n\
         elt.textContent = elt.textContent.sub('show','hide');\n\
         }\n\
       }\n\
//-->\n\
</script>\n\
<a href=\"javascript::void(0)\"\n\
   onclick=\"toggle_css_ident(event,'.mpost')\"\n\
   >\n\
hide mpost</a>\n\
<a href=\"javascript:void(0)\" \n\
   onclick=\"toggle_css_ident(event,'.png_cairo')\"\n\
   >\n\
show cairo png</a>\n\
<a href=\"javascript:void(0)\" \n\
   onclick=\"toggle_css_ident(event,'.mps')\"\n\
   >\n\
show mps</a>\n\
<a href=\"javascript:void(0)\" \n\
   onclick=\"toggle_css_ident(event,'.pgf')\"\n\
   >\n\
show pgf</a>\n\
<a href=\"javascript:void(0)\" \n\
   onclick=\"toggle_css_ident(event,'.pdf_cairo')\"\n\
   >\n\
show cairo pdf</a>\n\
<a href=\"javascript:void(0)\" \n\
   onclick=\"toggle_css_ident(event,'.svg_cairo')\"\n\
   >\n\
show cairo svg</a>\n\
\n\
"
}

let alpha_lower = ['a'-'z' ]
let alpha_upper = ['A'-'Z']
let alpha = ['a' - 'z' 'A'-'Z']
let digit = ['0'-'9']
let identifier = alpha_lower (alpha | digit | '\'' | '_')*
let blank = [' ' '\t' '\n' '\r' ]

rule scan fmt = parse
  | "<<togglescript>>" { Format.fprintf fmt "%s" togglescript; scan fmt lexbuf }
  | ">>" { Format.fprintf fmt "</p> </div><hr>"; scan fmt lexbuf }
  | "<<" (identifier as i)
      { 
       
        Format.fprintf fmt 
"<div class=\"table mpost\" title=\"with mpost : -ps\">\
<img src=\"ps_%s.png\" /></div>" i;
        Format.fprintf fmt 
"<div class=\"table mps\" title=\"with mps : -mps\" style=\"display:none;\">\
<img src=\"mps_%s.png\" /></div>" i;
        Format.fprintf fmt 
"<div class=\"table pgf\" title=\"with pgf : -pgf\" style=\"display:none;\">\
<img src=\"pgf_%s.png\" /></div>" i;
        Format.fprintf fmt 
"<div class=\"table png_cairo\" style=\"display:none;\">\
<img title=\"with cairo : -cairo -png\" src=\"png_cairo_%s.png\" /></div>" i;
        Format.fprintf fmt 
"<div class=\"table pdf_cairo\" style=\"display:none;\">\
<img title=\"with cairo : -cairo -pdf\" src=\"pdf_cairo_%s.png\" /></div>" i;
        Format.fprintf fmt 
"<div class=\"table svg_cairo\" style=\"display:none;\">\
<embed title=\"with cairo : -cairo -svg\" src=\"svg_cairo_%s.svg\" \
                                         type=\"image/svg+xml\"/>\
</div>" i;
        Format.fprintf fmt "<div style=\"clear:both;\">";
        Format.fprintf fmt "<a href=\"javascript:toggle_visibility('%s')\">" i;
        Format.fprintf fmt "show/hide code</a>";
        Format.fprintf fmt "</div>";
        Format.fprintf fmt "<div id=\"%s\" style=\"display:none;\" >" i;
        Format.fprintf fmt "<p>";
        scan fmt lexbuf
      }
  | blank { scan fmt lexbuf }
  | eof { Format.fprintf fmt "%!" }


{
  let parse_handler s =
    let buf = Lexing.from_string s in
    let s = Format.asprintf "%a" scan buf in
    Some s

let () = Caml2html.Plugin.add "parse" (`Function(parse_handler))

let () =
  let open Caml2html.Output in

  let file = Sys.argv.(1) in
  let buf = Buffer.create 8192 in
  let param = { default_param with
                html_comments = true;
                style = `Url "style.css";
  } in
  begin_document ~param buf [file];
  handle_file ~param buf file;
  end_document ~param buf;
  save_file buf (file ^ ".html")

}
