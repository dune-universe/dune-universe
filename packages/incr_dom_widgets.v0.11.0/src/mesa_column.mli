open! Core_kernel
open! Import

module Id : sig
  type t
  include Identifiable with type t := t
  val of_index : int -> t
end

type 'row t

val create
  :  header       : Vdom.Node.t
  -> column_id    : Id.t
  -> cells        : 'row Mesa_cell.Packed.t list
  -> ?style       : Css.t
  -> ?sort_by     : ('row -> Table.Default_sort_spec.Sort_key.t)
  -> ?group       : string
  -> ?cell_layout : [ `Vertical | `Horizontal ]  (* default is `Vertical *)
  -> unit
  -> 'row t

val header : _ t -> Vdom.Node.t
val cells  : 'row t -> 'row Mesa_cell.Packed.t Mesa_cell.Id.Map.t

val sort_by : 'row t -> ('row -> Table.Default_sort_spec.Sort_key.t) option

val group : _ t -> string option

val view
  :  'row t
  -> 'row Incr.t
  -> Mesa_cell.Mode.t Incr.t
  -> cell_html_id:(Mesa_cell.Id.t -> string)
  -> remember_edit:(Mesa_cell.Id.t -> string -> Vdom.Event.t)
  -> Row_node_spec.Cell.t Incr.t

val creator
  :  (column_id:Id.t -> 'row t) list
  -> 'row t Id.Map.t

val quick_header
  :  ?sep:Vdom.Node.t  (* default is line-break *)
  -> string list
  -> Vdom.Node.t
