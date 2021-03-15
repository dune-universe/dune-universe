(** The type of tables *)
type t = string Agrid.t

(** The signature of modules expected by the Make functor *)
module type Config = sig

  (** The initial table *)
  val initial : t Option.t

  (** The HTML id of the table *)
  val table_id : string

  (** The HTML id of the button to add a row *)
  val add_row_button_id : string Option.t

  (** The HTML id of the button to add a column *)
  val add_col_button_id : string Option.t

  (** The HTML id of the button to remove a row *)
  val rm_row_button_id : string Option.t

  (** The HTML id of the button to remove a column *)
  val rm_col_button_id : string Option.t

  (** The HTML id of the button to reset the table to its initial value *)
  val reset_button_id : string Option.t

  (** The HTML id of the button to run a user-defined action *)
  val action_button_id : string Option.t

  (** The user-defined action which will be run with the action button *)
  val action_fun : (t -> unit) Option.t

end

(** The signature of modules produced by the Make functor *)
module type S = sig

  (** The function to initialize the table, must be called once the page is loaded *)
  val init : unit -> unit

  (** Return the current content of the table *)
  val get_content : unit -> t

  (** Return the current dimensions of the table *)
  val dim : unit -> int * int

  (** Add an empty column to the table *)
  val add_col : unit -> unit

  (** Add an empty row to the table *)
  val add_row : unit -> unit

  (** Remove a column to the table *)
  val rm_col : unit -> unit

  (** Remove a row to the table *)
  val rm_row : unit -> unit

  (** Reset the table to its initial value *)
  val reset : unit -> unit

end

(** The Make functor *)
module Make (Config : Config) : S = struct

  let initial =
    let default = Agrid.of_list [[""]] in
    match Config.initial with
    | None -> default
    | Some initial ->
      match Agrid.dim initial with
      | _n, 0 | 0, _n -> default
      | _dim -> initial

  let a = ref initial

  let dim () = Agrid.dim !a

  open Js_of_ocaml

  let get_el ~id typ ~err =
    let el = Dom_html.window##.document##getElementById (Js.string id) in
    Js.coerce_opt el typ (fun _el -> Format.eprintf "%s@." err; assert false)

  let table =
    get_el ~id:Config.table_id Dom_html.CoerceTo.table ~err:(Format.sprintf "can't find table with id `%s`" Config.table_id)

  let get_button id =
    Some (get_el ~id Dom_html.CoerceTo.button ~err:(Format.sprintf "can't find button with id `%s`" id))

  let add_row_button = Option.bind Config.add_row_button_id get_button

  let add_col_button = Option.bind Config.add_col_button_id get_button

  let rm_row_button = Option.bind Config.rm_row_button_id get_button

  let rm_col_button = Option.bind Config.rm_col_button_id get_button

  let reset_button = Option.bind Config.reset_button_id get_button

  let action_button = Option.bind Config.action_button_id get_button

  let add_col () =
    let col = Flex_array.init (Agrid.height !a) (fun _i -> "") in
    a := Agrid.snoc_col !a col

  let add_row () =
    let row = Flex_array.init (Agrid.width !a) (fun _i -> "") in
    a := Agrid.snoc_row !a row

  let rm_col () =
    if Agrid.width !a > 1 then a := Agrid.liat_col !a

  let rm_row () =
    if Agrid.height !a > 1 then a := Agrid.liat_row !a

  let update_cel ~x ~y content =
    a := Agrid.set !a ~x ~y content

  let reset () =
    a := initial

  let ppi ?(pp_sep = Format.pp_print_cut) pp_v fmt a =
    let len = Flex_array.length a in
    for i = 0 to len - 2 do
      pp_v fmt ((Flex_array.get a i), i);
      pp_sep fmt ();
    done;
    if len > 0 then pp_v fmt ((Flex_array.get a (len - 1)), (len - 1))

  let pp_cel row_num fmt (cel, col_num) =
    Format.fprintf fmt {|<td contenteditable="true" id="row%d-col%d">%a</td>|} row_num col_num Format.pp_print_text cel

  let pp_row fmt (row, row_num) =
    Format.fprintf fmt {|<tr>%a</tr>|} (ppi (pp_cel row_num)) row

  let pp fmt (a : t) =
    Format.fprintf fmt {|%a|} (ppi pp_row) a

  let set_table () =
    table##.innerHTML := Js.string @@ Format.asprintf "%a" pp !a

  let link_button button f =
    match button with
    | None -> ()
    | Some button ->
      let _id = Dom_html.addEventListener button Dom_html.Event.click (Dom_html.handler (fun event -> f event; Js.bool true)) (Js.bool false) in ()

  let get_content () = !a

  let init () =
    set_table ();
    let _id =
      Dom_html.addEventListener table Dom_html.Event.input
        (Dom_html.handler (fun event ->
            let elt = Js.coerce_opt event##.target Dom_html.CoerceTo.td (fun _el -> Format.eprintf "can't get class of element@."; assert false) in
            let pos = Js.to_string elt##.id in
            let y, x = Scanf.sscanf pos "row%d-col%d" (fun row col -> row, col) in
            let content = Js.to_string elt##.innerHTML in
            let content = String.split_on_char '\n' content in
            let content = String.concat "" content in
            update_cel ~x ~y content;
            Js.bool true)
        ) (Js.bool false) in
    link_button rm_col_button (fun _event -> rm_col (); set_table ());
    link_button rm_row_button (fun _event -> rm_row (); set_table ());
    link_button add_col_button (fun _event -> add_col (); set_table ());
    link_button add_row_button (fun _event -> add_row (); set_table ());
    link_button reset_button (fun _event -> reset (); set_table ());
    link_button action_button (fun _event -> match Config.action_fun with None -> () | Some f -> f !a)
end
