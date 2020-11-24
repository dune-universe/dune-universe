
type field_name = (string * int)

let field_name_to_string f = 
  let (name, count) = f in 
  "(" ^ name ^ ", " ^ (string_of_int count) ^ ")"

let real_field_names field_packets = 
  let one_packet acc p = 
    let (l, count) = acc in
    let name = p.Mp_field_packet.field_name in
    ((name, count) :: l, count + 1)
  in
  let (l, _) = List.fold_left one_packet ([], 0) field_packets in
  List.rev l
