module  Statement : sig
  type t
  val _new_ : t
  val genome : string -> t
  val load :
    ?format:string ->
    ?index:string ->
    ?name:string ->
    string ->
    t
end

val script_of_statements : Statement.t list -> string

class proxy :
  ?addr:string ->
  ?port:int ->
  unit ->
  object
    method _new_ : (unit, string) result
    method genome : string -> (unit, string) result
    method load :
      ?format:string ->
      ?index:string ->
      ?name:string ->
      string ->
      (unit, string) result
  end
