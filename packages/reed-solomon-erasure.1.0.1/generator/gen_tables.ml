open Printf

let field_size : int = 256

let generating_polynomial : int = 29

let num_per_line = 16

let print_end_for_num (index : int) : unit =
  if (index + 1) mod num_per_line = 0 then
    printf "\n"
  else
    printf " "

let fprintf_end_for_num (oc : out_channel) (index : int) : unit =
  if (index + 1) mod num_per_line = 0 then
    fprintf oc "\n"
  else
    fprintf oc " "

let assert_val_in_range (v : int) : int =
  assert (0 <= v && v < 256);
  v

let validate_1D_table (table : int array) : int array =
  Array.iter
    (fun x -> assert_val_in_range x |> ignore)
    table;
  table

let validate_2D_table (table : int array array) : int array array =
  Array.iter
    (fun t ->
       Array.iter (fun x -> assert_val_in_range x |> ignore) t)
    table;
  table

let gen_log_table (polynomial : int) : int array =
  let result : int array = Array.make field_size 0 in
  let b      : int ref   = ref 1 in

  for log = 0 to (field_size-1) - 1 do
    result.(!b) <- assert_val_in_range log;

    b := !b lsl 1;

    if field_size <= !b then (
      b := (!b - field_size) lxor polynomial;
    )
  done;

  result

let exp_table_size : int = field_size * 2 - 2

let gen_exp_table (log_table : int array) : int array =
  let result : int array = Array.make exp_table_size 0 in

  for i = 1 to (field_size) - 1 do
    let log = log_table.(i) in
    result.(log)                  <- assert_val_in_range i;
    result.(log + field_size - 1) <- assert_val_in_range i;
  done;

  result

let multiply
    (log_table : int array)
    (exp_table : int array)
    (a : int)
    (b : int)
  : int =
  if a = 0 || b = 0 then
    0
  else (
    let log_a = log_table.(a) in
    let log_b = log_table.(b) in
    let log_result = log_a + log_b in
    exp_table.(log_result)
  )

let gen_mul_table
    (log_table : int array)
    (exp_table : int array)
  : int array array =
  let result : int array array = Array.make field_size [||] in

  for a = 0 to (field_size) - 1 do
    result.(a) <- Array.make field_size 0;
    for b = 0 to (field_size) - 1 do
      result.(a).(b) <- assert_val_in_range (multiply log_table exp_table a b);
    done
  done;

  result

let gen_mul_table_half
    (log_table : int array)
    (exp_table : int array)
  : int array array * int array array =
  let half_table_row_count  = field_size in
  let half_table_col_count  = 16 in

  let low  : int array array = Array.make half_table_row_count [||] in
  let high : int array array = Array.make half_table_row_count [||] in

  for a = 0 to (half_table_row_count) - 1 do
    low .(a) <- Array.make half_table_col_count 0;
    high.(a) <- Array.make half_table_col_count 0;

    for b = 0 to (half_table_row_count) - 1 do
      let result : int ref = ref 0 in
      if not (a = 0 || b = 0) then (
        let log_a = log_table.(a) in
        let log_b = log_table.(b) in
        result := exp_table.(log_a + log_b);
      );
      if b land 0x0F = b then (
        low.(a).(b)        <- assert_val_in_range !result;
      );
      if b land 0xF0 = b then (
        high.(a).(b lsr 4) <- assert_val_in_range !result;
      )
    done
  done;

  (low, high)

let print_tables_debug () : unit =
  let log_table = gen_log_table generating_polynomial in
  let exp_table = gen_exp_table log_table in
  let mul_table = gen_mul_table log_table exp_table in
  let (mul_table_low, mul_table_high) = gen_mul_table_half log_table exp_table in

  print_string "log table : [";
  for i = 0 to (Array.length log_table) - 1 do
    printf "%d, " log_table.(i);
    print_end_for_num i;
  done;
  print_endline "]";

  print_string "exp table : [";
  for i = 0 to (Array.length exp_table) - 1 do
    printf "%d, " exp_table.(i);
    print_end_for_num i;
  done;
  print_endline "]";

  print_string "mul table : [";
  for i = 0 to (Array.length mul_table) - 1 do
    print_string "[";
    for j = 0 to (Array.length mul_table.(i)) - 1 do
      printf "%d, " mul_table.(i).(j);
      print_end_for_num j;
    done;
    print_endline "],";
  done;
  print_endline "]";

  print_string "mul table low : [";
  for i = 0 to (Array.length mul_table_low) - 1 do
    print_string "[";
    for j = 0 to (Array.length mul_table_low.(i)) - 1 do
      printf "%d, " mul_table_low.(i).(j);
      print_end_for_num j;
    done;
    print_endline "],";
  done;
  print_endline "]";

  print_string "mul table high : [";
  for i = 0 to (Array.length mul_table_high) - 1 do
    print_string "[";
    for j = 0 to (Array.length mul_table_high.(i)) - 1 do
      printf "%d, " mul_table_high.(i).(j);
      print_end_for_num j;
    done;
    print_endline "],";
  done;
  print_endline "]"

let write_1D_table (oc : out_channel) (name : string) (table : int array) : unit =
  fprintf oc "let %s : (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t = Bigarray.Array1.of_array Bigarray.char Bigarray.c_layout [|\n" name;
  for i = 0 to (Array.length table) - 1 do
    fprintf oc "'\\%03d';" table.(i);
    fprintf_end_for_num oc i;
  done;
  fprintf oc "|]\n"

let write_2D_table (oc : out_channel) (name : string) (table : int array array) : unit =
  let rows = Array.length table in
  let cols = Array.length table.(0) in

  fprintf oc "let %s : (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array2.t = Bigarray.Array2.of_array Bigarray.char Bigarray.c_layout [|\n" name;
  for i = 0 to rows - 1 do
    fprintf oc "[|";
    for j = 0 to cols - 1 do
      fprintf oc "'\\%03d';" table.(i).(j);
      fprintf_end_for_num oc j;
    done;
    fprintf oc "|];\n";
  done;
  fprintf oc "|]\n"

let main () =
  let log_table = validate_1D_table (gen_log_table generating_polynomial) in
  let exp_table = validate_1D_table (gen_exp_table log_table) in
  let mul_table = validate_2D_table (gen_mul_table log_table exp_table) in
  let (mul_table_low, mul_table_high) =
    (let (mul_table_low, mul_table_high) =
       gen_mul_table_half log_table exp_table
     in
     (validate_2D_table mul_table_low, validate_2D_table mul_table_high))
  in

  let oc = open_out "tables.ml" in

  write_1D_table oc "log_table"      log_table;
  write_1D_table oc "exp_table"      exp_table;
  write_2D_table oc "mul_table"      mul_table;
  write_2D_table oc "mul_table_low"  mul_table_low;
  write_2D_table oc "mul_table_high" mul_table_high;

  close_out oc
;;

main ()
