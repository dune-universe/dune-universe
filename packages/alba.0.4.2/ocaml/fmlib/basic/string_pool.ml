module String_map = Finite_map.Make (struct
                                      type t = string
                                      let compare = Stdlib.compare
                                    end)

module IString =
  struct
    type t = {id:int; str: string}
    let make (id:int) (str:string) =  {id;str}
    let id (s:t): int = s.id
    let string (s:t): string = s.str
    let compare (a:t) (b:t): int =
      compare a.id b.id
  end

type t = { count:int; map: IString.t String_map.t }

let empty = {count = 0;  map = String_map.empty}

let intern (s:string) (sp:t): IString.t * t =
  match String_map.maybe_find s sp.map with
  | None ->
     let is = IString.make sp.count s in
     let map = String_map.add s is sp.map in
     is,
     {count = sp.count+1; map}
  | Some is ->
     is, sp
