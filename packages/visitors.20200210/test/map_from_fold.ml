(* Direct definitions of [map], [reduce], and [fold]. *)
class virtual ['self] reduce = object (self: 'self)
  method private visit_option: 'a .
    ('env -> 'a -> 'z) -> 'env -> 'a option -> 'z
  = fun f env ox ->
      match ox with None -> self#zero | Some x -> f env x
  method private virtual zero: 'z
end
class ['self] map = object (_ : 'self)
  method private visit_option: 'a 'b .
    ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
  = fun f env ox ->
      match ox with None -> None | Some x -> Some (f env x)
end
class virtual ['self] fold = object (self : 'self)
  method private visit_option: 'a .
    ('env -> 'a -> 'r) -> 'env -> 'a option -> 's
  = fun f env ox ->
      match ox with
      | None   -> self#build_None env
      | Some x -> self#build_Some env (f env x)
  method private virtual build_None: 'env -> 's
  method private virtual build_Some: 'env -> 'r -> 's
end
(* A successful definition of [reduce] in terms of [fold]. *)
class virtual ['self] reduce_from_fold = object (self : 'self)
  inherit [_] fold
  method private build_None _env   = self#zero
  method private build_Some _env z = z
  method private virtual zero: 'z
end
(* An unsatisfactory definition of [map] in terms of [fold]. *)
class ['self] map_from_fold = object (_ : 'self)
  inherit [_] fold
  method private build_None _env   = None
  method private build_Some _env x = Some x
end
