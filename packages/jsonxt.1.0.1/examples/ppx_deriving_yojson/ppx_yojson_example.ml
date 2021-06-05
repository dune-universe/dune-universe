module Item = struct
  type t = {
    str : string
  ; cost : float
  } [@@deriving yojson]
end

module Stock = struct
  type t = {
    desc : string
  ; inventory : int
  ; backorder : int option
  ; items : Item.t list
  } [@@deriving yojson]
end

let () =
  let item1 = { Item.str = "Store Baked Beans"; cost = 1.22 } in
  let item2 = { Item.str = "Branded Baked Beans"; cost = 1.47 } in
  let stock = { Stock.desc = "Beans"; inventory = 2; backorder = Some 3; items = [item1; item2] } in
  let json = Stock.to_yojson stock in
  print_endline (Yojson.Safe.show json);
  print_endline (Yojson.Safe.pretty_to_string json);

