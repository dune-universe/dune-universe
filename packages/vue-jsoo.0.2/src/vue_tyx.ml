open Tyxml.Html

let vattr key value = to_attrib @@ Xml.string_attrib key value

let v_ s = txt @@ "{{ " ^ s ^ " }}"
let v_bind key value = vattr ("v-bind:" ^ key) value
let v_if value = vattr "v-if" value
let v_else () = vattr "v-else" ""
let v_else_if value = vattr "v-else-if" value
let v_show value = vattr "v-show" value
let v_for item array = vattr "v-for" (item ^ " in " ^ array)
let v_on event action = vattr ("v-on:" ^ event) action
let v_html value = vattr "v-html" value
let v_slot prop = vattr ("v-slot:" ^ prop) ""
