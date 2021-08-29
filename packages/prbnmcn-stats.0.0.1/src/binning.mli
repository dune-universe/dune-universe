(** Binning finitely supported distributions *)

(** The type of binning specifications (shape of the grid, etc) *)
type spec

(** [regular ~origin ~width ~truncate] specifies a regular grid with cells
    aligned on [origin] and spaced by [width]. Cells are indexed,
    with the cell [[origin, origin+width]] having index 0.
    If [truncate] is equal to [Some (l, r)], the points outside of the
    (inclusive) [[l, r]] interval will be discarded.

    @raise Invalid_argument if [width <= 0] or if [truncate = Some (l, r)]and
    the [[l, r]] interval is empty. *)
val regular :
  origin:float -> width:float -> truncate:(float * float) option -> spec

(** [compute spec mes] bins the mesure on the grid specified by [spec].
    After binning, the measure is supported by grid indices corresponding
    to the specification. *)
val compute : spec -> float Fin.Float.mes -> int Fin.Float.mes
