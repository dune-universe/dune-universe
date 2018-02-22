
module Document = Document

module C = Jupyter_kernel.Client
module H = Tyxml.Html

type mime_data = Jupyter_kernel.Client.mime_data = {
  mime_type: string;
  mime_content: string;
  mime_b64: bool;
}

type 'a mime_printer = 'a -> mime_data

(* display a document as HTML *)
let to_html : Document.t -> [<Html_types.div] H.elt =
  let mk_header ~depth l = match depth with
    | 1 -> H.h1 l
    | 2 -> H.h2 l
    | 3 -> H.h3 l
    | 4 -> H.h4 l
    | 5 -> H.h5 l
    | n when n>=6 -> H.h6 l
    | _ -> assert false
  in
  let rec aux ~depth doc =
    H.div (List.map (aux_block ~depth) doc)
  and aux_block ~depth (b:Document.block) =
    let h = match b with
      | `S s -> mk_header ~depth [H.pcdata s]
      | `P s -> H.p [H.pcdata s]
      | `Pre s -> H.pre [H.pcdata s]
      | `L l ->
        H.ul (List.map (fun sub -> H.li [aux ~depth sub]) l)
      | `I (s,sub) ->
        let depth = depth+1 in
        H.div (
          mk_header ~depth [H.pcdata s] :: List.map (aux_block ~depth) sub
        )
      | `Tbl (heads, rows) ->
        let thead =
          let l = List.map (fun s -> H.th [H.pcdata s]) heads in
          H.thead [H.tr l]
        and rows =
          let depth=depth+1 in
          List.map
            (fun row -> H.tr (List.map (fun s -> H.td [aux ~depth s]) row))
            rows
        in
        H.table ~thead rows
    in
    H.div [h]
  in
  aux ~depth:3

let mime_of_html (h:_ H.elt) : C.mime_data =
  let s = Format.asprintf "%a@." (H.pp_elt ()) h in
  {C.mime_type="text/html"; mime_content=s; mime_b64=false}
