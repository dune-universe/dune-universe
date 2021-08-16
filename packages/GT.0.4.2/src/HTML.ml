(**************************************************************************
 *  Copyright (C) 2005-2021
 *  Dmitri Boulytchev (db@tepkom.ru), St.Petersburg State University
 *  Universitetskii pr., 28, St.Petersburg, 198504, RUSSIA
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA
 *
 *  See the GNU Lesser General Public License version 2.1 for more details
 *  (enclosed in the file COPYING).
 **************************************************************************)

open Printf

type er = View.er
type viewer = er

let ref' = ref

let toHTML = View.toString

let escape s =
  let buf = Buffer.create (String.length s * 2) in
  for i=0 to String.length s - 1 do
    Buffer.add_string buf
      (match s.[i] with
      | '<' -> "&lt;"
      | '>' -> "&gt;"
      | '&' -> "&amp;"
      | '"' -> "&quot;"
      | c   -> String.make 1 c
      )
  done;
  Buffer.contents buf

let string s = View.string (escape s)
let raw    s = View.string s

let unit   = View.unit
let int    = View.int
let float  = View.float
let bool   = View.bool
let char   = View.char

let seq    = View.seq
let seqa   = View.seqa

let br = raw "<br>"

let tag ?(attrs="") s p =
  seq [raw (sprintf "<%s>" (s ^ (if attrs = "" then "" else " ") ^ attrs)); p; raw (sprintf "</%s>" s)]

let link url =
  seq
    [ raw @@
      sprintf "<link rel=\"stylesheet\" type=\"text/css\" href=\"%s\" media=\"screen\">" url
    ]

let html  ?(attrs="") = tag "html"  ~attrs:attrs
let title ?(attrs="") = tag "title" ~attrs:attrs
let body  ?(attrs="") = tag "body"  ~attrs:attrs
let ul    ?(attrs="") = tag "ul"    ~attrs:attrs
let ol    ?(attrs="") = tag "ol"    ~attrs:attrs
let li    ?(attrs="") = tag "li"    ~attrs:attrs
let b     ?(attrs="") = tag "b"     ~attrs:attrs
let i     ?(attrs="") = tag "i"     ~attrs:attrs
let table ?(attrs="") = tag "table" ~attrs:attrs
let tr    ?(attrs="") = tag "tr"    ~attrs:attrs
let td    ?(attrs="") = tag "td"    ~attrs:attrs
let th    ?(attrs="") = tag "th"    ~attrs:attrs
let form  ?(attrs="") = tag "form"  ~attrs:attrs
let input ?(attrs="") = tag "input" ~attrs:attrs

let checkbox ?(attrs="") = tag "input"    ~attrs:(sprintf "%s type=\"checkbox\"" attrs)
let button   ?(attrs="") = tag "button"   ~attrs:(sprintf "%s type=\"button\""   attrs)
let div      ?(attrs="") = tag "div"      ~attrs:(sprintf "%s contentEditable=\"true\"" attrs)
let text     ?(attrs="") = tag "input"    ~attrs:(sprintf "%s type=\"text\"" attrs)
let textarea ?(attrs="") = tag "textarea" ~attrs:attrs

let radio ?(attrs="") triples =
  seq (List.map (fun (name, v, a) -> seq [tag "input" ~attrs:(sprintf "%s %s type=\"radio\" value=\"%s\"" attrs a v) name; raw "&nbsp;"])
        triples
      )

let select ?(attrs="") triples =
  tag "select" ~attrs:attrs (
    seq (List.map (fun (name, v, a) -> tag "option" ~attrs:(sprintf "%s value=\"%s\"" a v) name)
          triples
        )
  )

let anchor r p = seq [raw (sprintf "<a name=%S>" r); p; raw "</a>"]
let ref    r p = seq [raw (sprintf "<a href=%S>" r); p; raw "</a>"]

let named n p = seq [b (string (n ^ ": ")); p]

let list  p = tag "ul" (seq  (List .map (tag "li") p))
let array p = tag "ul" (seqa (Array.map (tag "li") p))

let fields l = list (List.map (fun (n, x) -> named n x) l)

let make f x = raw (f x)

module Wizard =
  struct

    module Page =
      struct

        module Item =
          struct

            type typ =
	      | String of string
	      | Text   of string * string
	      | Div    of string * string
	      | Flag   of string
	      | Select of string * (viewer * string * string) list
	      | Radio  of string * (viewer * string * string) list

            type t = {name: string; id: string; typ: typ}

            let make id name typ = {name=name; id=sprintf "%s_%s" id name; typ=typ}

            let render t =
              let attrs' attrs = sprintf "%s id=\"%s\"" attrs t.id in
              t.id,
              (seq [
                td ~attrs:"align=\"right\" valign=\"center\"" (raw t.name);
                td ~attrs:"align=\"center\" valign=\"center\"" (raw ":");
                td ~attrs:"align=\"left\" valign=\"center\"" (
                  match t.typ with
		  | String  attrs -> text ~attrs:(attrs' attrs) View.empty
                  | Text   (attrs, text) -> textarea ~attrs:(attrs' attrs) (raw text)
		  | Div    (attrs, text) -> div ~attrs:(attrs' attrs) (raw text)
                  | Flag    attrs -> checkbox ~attrs:(attrs' attrs) View.empty
                  | Select (attrs, triples) -> select ~attrs:(attrs' attrs) triples
		  | Radio  (attrs, triples) -> radio ~attrs:(attrs' (sprintf "%s name=\"%s\"" attrs t.id)) triples
                )
              ])

          end

        class c id attrs =
          object(this)
            val mutable items : Item.t list = []
            method add name item =
	      items <- (Item.make id name item) :: items;
	      this
            method text   ?(attrs="") ?(default="" ) name = this#add name (Item.Text   (attrs, default))
            method div    ?(attrs="") ?(default="" ) name = this#add name (Item.Div    (attrs, default))

            method string ?(attrs="") name = this#add name (Item.String attrs)
            method flag   ?(attrs="") name = this#add name (Item.Flag   attrs)
            method combo  ?(attrs="") name items = this#add name (Item.Select (attrs, items))
            method radio  ?(attrs="") name items = this#add name (Item.Radio  (attrs, items))

            method id name = (List.find (fun i -> i.Item.name = name) items).Item.id
	    method render (back, backA, backCb) (next, nextA, nextCb) =
              let ids, rendered =
	        List.split (
	          List.map
		    (fun t -> let id, r = Item.render t in (t.Item.name, id, t.Item.typ), r)
                    (List.rev items)
                )
	      in
              let html =
                table ~attrs:attrs (
                  seq (
                    (List.map tr rendered) @
                    [tr (td ~attrs:"colspan=\"3\"" (raw "<hr>"));
                     tr (td ~attrs:"colspan=\"3\" align=\"center\""
                           (seq [
                             button ~attrs:(sprintf "%s onclick=\"%s\"" backA backCb) (raw back);
                             raw "&nbsp;&nbsp;";
                             button ~attrs:(sprintf "%s onclick=\"%s\"" nextA nextCb) (raw next)
                           ])
                     )
                    ]
	          )
                )
	      in
              let savef = sprintf "save_%s" id in
              let loadf = sprintf "load_%s" id in
              let js = Buffer.create 1024 in
              let generate s = Buffer.add_string js s in
	      let innerText elem gen =
		generate (sprintf "if (typeof %s.innerText === \"undefined\") {" elem);
                generate (sprintf "  %s" (gen (elem ^ ".textContent")));
                generate ("}\n");
                generate ("else {\n");
                generate (sprintf "  %s" (gen (elem ^ ".innerText")));
                generate ("}\n")
	      in
              generate (sprintf "function %s (curr) {\n" loadf);
                generate "  var coll = null;\n";
                List.iter
                  (fun (name, id, t) ->
                     match t with
                     | Item.Flag _ -> generate (sprintf "  if (curr[\"%s\"]) document.getElementById (\"%s\").checked = curr[\"%s\"];\n" name id name);
		     | Item.Div _ ->
                         innerText (sprintf "document.getElementById (\"%s\")" id)
                                   (fun elem -> sprintf "  if (curr[\"%s\"]) %s = curr[\"%s\"];\n" name elem name)
                     | Item.Radio _ ->
                         generate (sprintf "  if (curr[\"%s\"]) {\n"  name);
                         generate (sprintf "    coll = document.getElementsByName (\"%s\");\n" id);
                         generate          "    for (var i = 0; i<coll.length; i++) {\n";
                         generate (sprintf "      coll[i].checked = coll[i].value == curr[\"%s\"];\n" name);
                         generate          "    }\n";
                         generate          "  }\n"
                     | _ -> generate (sprintf "  if (curr[\"%s\"]) document.getElementById (\"%s\").value = curr[\"%s\"];\n" name id name);
                  )
                  ids;
                generate "}\n";
              generate (sprintf "function %s (curr) {\n" savef);
                generate "  var coll = null;\n";
                List.iter
                  (fun (name, id, t) ->
                     match t with
                     | Item.Flag  _ -> generate (sprintf "  curr[\"%s\"] = document.getElementById(\"%s\").checked;\n" name id)
                     | Item.Div   _ ->
                         innerText (sprintf "document.getElementById(\"%s\")" id)
                                   (fun elem -> sprintf "  curr[\"%s\"] = %s.replace(/\\u00a0/g, \" \");\n" name elem)
                     | Item.Radio _ ->
                       generate (sprintf "  coll = document.getElementsByName (\"%s\");\n" id);
                       generate          "  for (var i = 0 ; i<coll.length; i++) {\n";
                       generate          "    if (coll[i].checked) {\n";
                       generate (sprintf "       curr[\"%s\"] = coll[i].value;\n" name);
                       generate          "       break;\n";
                       generate          "    }\n";
                       generate          "  }\n";
                     | _ -> generate (sprintf "  curr[\"%s\"] = document.getElementById(\"%s\").value;\n" name id)
                  )
                  ids;
                generate "}\n";
              savef, loadf, Buffer.contents js, html
          end

      end

    type page = < string : ?attrs:string -> string -> page;
                  text   : ?attrs:string -> ?default:string -> string -> page;
                  div    : ?attrs:string -> ?default:string -> string -> page;
                  flag   : ?attrs:string -> string -> page;
                  combo  : ?attrs:string -> string -> (viewer * string * string) list -> page;
                  radio  : ?attrs:string -> string -> (viewer * string * string) list -> page;
                  id     : string -> string;
                >

    type t = < page : (page -> page) list -> page; generate : string * string >

    let string ?(attrs="") name       (p:page) = p#string ~attrs:attrs name
    let flag   ?(attrs="") name       (p:page) = p#flag   ~attrs:attrs name
    let combo  ?(attrs="") name items (p:page) = p#combo  ~attrs:attrs name items
    let radio  ?(attrs="") name items (p:page) = p#radio  ~attrs:attrs name items

    let text   ?(attrs="") ?(default="") name (p:page) = p#text ~attrs:attrs ~default:default name
    let div    ?(attrs="") ?(default="") name (p:page) = p#div  ~attrs:attrs ~default:default name

    let mapi f l =
      let rec inner i = function
      | []    -> []
      | h::tl -> f i h :: inner (i+1) tl
      in inner 0 l

    class c attrs id target navigate =
      object
        val mutable pages : Page.c list = []
        val mutable i : int = 0
        method page l =
          let p = new Page.c (sprintf "page_%s_%d" id i) attrs in
          pages <- p :: pages;
          i <- i+1;
          List.fold_left (fun p f -> f p) (p :> page) l

        method generate =
          let n  = List.length pages         in
          let bb = sprintf "bb_%s"        id in
          let nb = sprintf "nb_%s"        id in
          let pg = sprintf "page_%s"      id in
          let bf = sprintf "do_back_%s"   id in
          let nf = sprintf "do_next_%s"   id in
          let pc = sprintf "pages_%s"     id in
          let pr = sprintf "present_%s"   id in
          let sf = sprintf "savefs_%s"    id in
          let lf = sprintf "loadfs_%s"    id in
          let cr = sprintf "curr_%s"      id in
          let sv = sprintf "save_%s"      id in
          let ld = sprintf "load_%s"      id in
          let st = sprintf "stack_%s"     id in
          let sp = sprintf "stack_ptr_%s" id in
          let pu = sprintf "push_%s"      id in
          let po = sprintf "pop_%s"       id in
          let js = Buffer.create 1024        in
          let generate s = Buffer.add_string js s in
          let funs, pages =
            List.split (
              mapi (fun i p ->
                let savef, loadf, script, page =
                  p#render
                    ("Back", sprintf "id=\"%s\"" bb, bf ^ " ()")
                    ("Next", sprintf "id=\"%s\"" nb, nf ^ " ()")
                in
                generate script;
                (savef, loadf), toHTML page
              ) (List.rev pages)
            )
          in
          let savefs, loadfs = List.split funs in
          generate (sprintf "var %s = 0;\n" sp);
          generate (sprintf "var %s = new Array(%d);\n" st n);
          generate (sprintf "function %s (i) {\n" pu);
            generate (sprintf "  %s [%s++] = i;\n" st sp);
            generate "}\n";
          generate (sprintf "function %s () {\n" po);
            generate (sprintf "  return %s [--%s];\n" st sp);
            generate "}\n";
          generate (sprintf "var %s = {};\n" cr);
          generate (sprintf "var %s = 0;\n" pg);
          generate (sprintf "var %s = [\n" pc);
            List.iter
              (fun p -> generate (sprintf "  \"%s\",\n" (String.escaped p)))
              pages;
            generate "  \"\"];\n";
          generate (sprintf "var %s = [\n" sf);
            List.iter (fun s -> generate (sprintf "  %s,\n" s)) savefs;
            generate "  \"\"];\n";
          generate (sprintf "function %s () {\n" sv);
            generate (sprintf "  %s[%s] (%s);\n" sf pg cr);
            generate "}\n";
          generate (sprintf "var %s = [\n" lf);
            List.iter (fun s -> generate (sprintf "  %s,\n" s)) loadfs;
            generate "  \"\"];\n";
          generate (sprintf "function %s () {\n" ld);
            generate (sprintf "  %s[%s] (%s);\n" lf pg cr);
            generate "}\n";
          generate (sprintf "function %s () {\n" pr);
            generate (sprintf "  document.getElementById (\"%s\").innerHTML = %s[%s];\n" target pc pg);
            generate (sprintf "  %s (%s);\n" ld cr);
            generate (sprintf "  document.getElementById (\"%s\").disabled = 0 == %s;\n" bb pg);
            generate "}\n";
          generate (sprintf "function %s () {\n" bf);
            generate (sprintf "  %s (%s);\n" sv cr);
            generate (sprintf "  %s = %s ();\n" pg po);
            generate (sprintf "  %s ();\n" pr);
            generate "}\n";
          generate (sprintf "function %s () {\n" nf);
            generate (sprintf "  %s (%s);\n" sv cr);
            generate (sprintf "  var nxt = %s (%s, %s);\n" navigate pg cr);
            generate (sprintf "  if (nxt < 0 || nxt == %s) return;\n" pg);
            generate (sprintf "  %s (%s);\n" pu pg);
            generate (sprintf "  %s = nxt;\n" pg);
            generate (sprintf "  %s ();\n" pr);
            generate "}\n";
          (pr, Buffer.contents js)
      end

      let create ?(attrs="") id target navigate = new c attrs id target navigate

  end

module type Element =
  sig

    type t

    val toHTML : t -> string

  end

module L = List

module String =
  struct

    type t = string

    let named  n v = toHTML (named n (raw v))
    let fields v   = toHTML (fields (List.map (fun (n, v) -> n, raw v) v))
    let anchor n v = toHTML (anchor n (raw v))
    let ref    n v = toHTML (ref    n (raw v))

    let toHTML s = s

  end

module Anchor (X : sig type t val name : string end) =
  struct

    module H = Hashtbl.Make
	(
	 struct

	   type t = X.t

	   let hash  = Hashtbl.hash
	   let equal = (==)

	 end
	)

    let h = H.create 1024
    let index =
      let i = ref' 0 in
      (fun () ->
	incr i;
	!i
      )

    let set x   = H.add h x (index ())
    let isSet x = H.mem h x
    let get x   =
      if not (isSet x) then set x;
      sprintf "%s.anchor%d" X.name (H.find h x)

    let url t = "#" ^ get t

    let ref t text = ref (url t) text

    module String =
      struct

	let ref t text = String.ref (url t) text

      end

  end

module Raw =
  struct

    type t = string

    let toHTML s = toHTML (raw s)

  end

open List

module List (T : Element) =
  struct

    type t = T.t list

    let toHTML l = toHTML (list (List.map (make T.toHTML) l))

  end

module Array (T : Element) =
  struct

    type t = T.t array

    let toHTML a = toHTML (array (Array.map (make T.toHTML) a))

  end

module NamedPair (N : sig val first : string val second : string end) (F : Element) (S : Element) =
  struct

    type t = F.t * S.t

    let toHTML (f, s) =
      toHTML
	(list
	   [named N.first  (make F.toHTML f);
            named N.second (make S.toHTML s);
	   ]
	)

  end

module Pair = NamedPair (struct let first = "" let second = "" end)

module Set (S : Set.S) (V : Element with type t = S.elt) =
  struct

    type t = S.t

    let toHTML x =
      let module LL = List (String) in
      LL.toHTML (L.sort Stdlib.compare (L.map V.toHTML (S.elements x)))

  end

module Map (M : Map.S) (K : Element with type t = M.key) (V : Element) =
  struct

    type t = V.t M.t

    let toHTML x =
      let module P  = NamedPair (struct let first = "key" let second = "value" end)(K)(V) in
      let module LL = List (String) in
      LL.toHTML (L.sort Stdlib.compare (M.fold (fun x y acc -> (P.toHTML (x, y)) :: acc) x []))

  end

module Hashtbl (M : Hashtbl.S) (K : Element with type t = M.key) (V : Element) =
  struct

    type t = V.t M.t

    let toHTML x =
      let module P  = NamedPair(struct let first = "key" let second = "value" end)(K)(V) in
      let module LL = List (String) in
      LL.toHTML (L.sort Stdlib.compare (M.fold (fun x y acc -> (P.toHTML (x, y)) :: acc) x []))

  end
