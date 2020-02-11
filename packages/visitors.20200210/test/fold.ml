type person = {
  firstname: string[@opaque];
  surname:   string[@opaque]
}

and crowd =
  | Nobody
  | Someone of person * crowd
[@@deriving visitors { variety = "fold" }]

let convert : crowd -> (string * string) list =
  let v = object
    inherit [_] fold
    method build_person  () f s = (f, s)
    method build_Nobody  ()     = []
    method build_Someone () p c = p :: c
  end
  in v # visit_crowd ()
