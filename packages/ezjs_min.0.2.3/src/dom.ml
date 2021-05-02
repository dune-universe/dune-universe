open Js

let setInnerHtml elt s = elt##.innerHTML := string s

let setText elt = function
  | None ->
      ()
  | Some s ->
      elt##.textContent := some (string s)

let addClass elt s = elt##.classList##add (string s)

let addClasses elt l = List.iter (addClass elt) l

let removeClass elt s = elt##.classList##remove (string s)

let containsClass elt s = elt##.classList##contains (string s)

let setAttribute elt key value = elt##setAttribute (string key) (string value)

let removeAttribute elt key = elt##removeAttribute (string key)

let getAttribute elt key = Opt.to_option (elt##getAttribute (string key))

let setCSS elt styles =
  let styles =
    String.concat "; " (List.map (fun (k, v) -> k ^ ": " ^ v) styles)
  in
  setAttribute elt "style" styles

let addCSS elt styles =
  let styles =
    String.concat "; " (List.map (fun (k, v) -> k ^ ": " ^ v) styles)
  in
  let styles =
    match getAttribute elt "style" with
    | None ->
        styles
    | Some old_styles ->
        old_styles ^ "; " ^ styles
  in
  setAttribute elt "style" styles

let appendChild = Dom.appendChild

let removeChild = Dom.removeChild

let appendChildren parent children = List.iter (Dom.appendChild parent) children

let children parent = Dom.list_of_nodeList parent##.childNodes

let removeChildi parent i =
  match List.nth_opt (children parent) i with
  | None ->
      ()
  | Some child ->
      removeChild parent child

let removeChildren parent = List.iter (removeChild parent) (children parent)

let replaceChildren parent children =
  removeChildren parent ;
  appendChildren parent children

let by_id s = Dom_html.getElementById s

let addListener elt ev f =
  ignore
  @@ Dom.addEventListener elt (Dom.Event.make ev)
       (Dom.handler (fun e -> bool (f e)))

module El = struct
  let create ?(classes = []) ?(styles = []) ?(listen = []) ?(attr = []) ?text f
      children =
    let elt = f Dom_html.document in
    List.iter (fun (ev, f) -> addListener elt ev f) listen ;
    addClasses elt classes ;
    setCSS elt styles ;
    List.iter (fun (k, v) -> setAttribute elt k v) attr ;
    setText elt text ;
    appendChildren elt children ;
    elt

  let button ?classes ?styles ?listen ?attr ?text children =
    create ?classes ?styles ?listen ?attr ?text Dom_html.createButton children

  let div ?classes ?styles ?listen ?attr ?text children =
    create ?classes ?styles ?listen ?attr ?text Dom_html.createDiv children

  let a ?classes ?styles ?listen ?attr ?text children =
    create ?classes ?styles ?listen ?attr ?text Dom_html.createA children

  let span ?classes ?styles ?listen ?attr ?text children =
    create ?classes ?styles ?listen ?attr ?text Dom_html.createSpan children

  let form ?classes ?styles ?listen ?attr ?text children =
    create ?classes ?styles ?listen ?attr ?text Dom_html.createForm children

  let option ?classes ?styles ?listen ?attr ?text children =
    create ?classes ?styles ?listen ?attr ?text Dom_html.createOption children

  let select ?classes ?styles ?listen ?attr ?text children =
    create ?classes ?styles ?listen ?attr ?text Dom_html.createSelect children

  let input ?classes ?styles ?listen ?attr ?text () =
    create ?classes ?styles ?listen ?attr ?text Dom_html.createInput []

  let iframe ?classes ?styles ?listen ?attr ?text children =
    create ?classes ?styles ?listen ?attr ?text Dom_html.createIframe children

  let label ?classes ?styles ?listen ?attr ?text children =
    create ?classes ?styles ?listen ?attr ?text Dom_html.createLabel children

  let ul ?classes ?styles ?listen ?attr ?text children =
    create ?classes ?styles ?listen ?attr ?text Dom_html.createUl children

  let li ?classes ?styles ?listen ?attr ?text children =
    create ?classes ?styles ?listen ?attr ?text Dom_html.createLi children

  let img ?classes ?styles ?listen ?attr ?text children =
    create ?classes ?styles ?listen ?attr ?text Dom_html.createImg children

  let script ?classes ?styles ?listen ?attr ?text children =
    create ?classes ?styles ?listen ?attr ?text Dom_html.createScript children

  let table ?classes ?styles ?listen ?attr ?text children =
    create ?classes ?styles ?listen ?attr ?text Dom_html.createTable children

  let tr ?classes ?styles ?listen ?attr ?text children =
    create ?classes ?styles ?listen ?attr ?text Dom_html.createTr children

  let th ?classes ?styles ?listen ?attr ?text children =
    create ?classes ?styles ?listen ?attr ?text Dom_html.createTh children

  let td ?classes ?styles ?listen ?attr ?text children =
    create ?classes ?styles ?listen ?attr ?text Dom_html.createTd children
end

let encapse s = "\"" ^ s ^ "\""

let strings_to_array l = "[" ^ String.concat "," l ^ "]"

let strings_to_object l =
  let s =
    "{"
    ^ String.concat "," (List.map (fun (k, v) -> encapse k ^ ":" ^ v) l)
    ^ "}"
  in
  try _JSON##parse (string s)
  with _ ->
    log_str ("cannot parse json " ^ s) ;
    Unsafe.obj [||]
