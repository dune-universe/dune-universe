type unit_ = (string * int) list

let unit_one = []

let unit_canonicalize (u: unit_): unit_ =
  let rec combine = function
    | [] -> []
    | [x] -> [x]
    | (name1, power1) :: (name2, power2) :: rest ->
        if name1 = name2 then
          combine ((name1, power1 + power2) :: rest)
        else
          (name1, power1) :: combine ((name2, power2) :: rest)
  in
  let nonzero_power = function
    | _, 0 -> false
    | _, _ -> true
  in
  u |> List.sort compare |> combine |> List.filter nonzero_power

let unit_mul (u1: unit_) (u2: unit_): unit_ =
  u1 @ u2

let unit_recip (u: unit_): unit_ =
  u |> List.map (fun (name, power) -> (name, -power))

let unit_div (u1: unit_) (u2: unit_): unit_ =
  u1 @ unit_recip u2

let string_of_unit (u: unit_): string =
  match u with
  | [] -> "1"
  | _ ->
      let string_of_power =
        (function
          | name, 1 -> name
          | name, power -> Printf.sprintf "%s^%i" name power)
      in
      u |> List.map string_of_power |> String.concat " "

let base_unit_power (base_unit: string) (u: unit_): int =
  try List.assoc base_unit u with Not_found -> 0

let all_base_units_in_unit (u: unit_) : string list =
  List.map fst u
