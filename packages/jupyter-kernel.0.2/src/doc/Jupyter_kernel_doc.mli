
module Document = Document

type mime_data = Jupyter_kernel.Client.mime_data = {
  mime_type: string;
  mime_content: string;
  mime_b64: bool;
}

type 'a mime_printer = 'a -> mime_data

val to_html : Document.t -> Html_types.div Tyxml.Html.elt

val mime_of_html : _ Tyxml.Html.elt -> Jupyter_kernel.Client.mime_data
