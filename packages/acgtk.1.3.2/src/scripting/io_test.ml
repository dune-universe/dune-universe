
let print_list () = 
  let () = Printf.printf "list;\n%!" in
  Unix.sleep 4
    

let get_and_print_line in_ch =
  let s = input_line in_ch in
  Printf.printf "%s\n%!" s


let read_and_output filename =
  let in_ch = 
    let fullname = Utils.find_file filename ["."]  in
    open_in fullname in
  try
    while true do
      let () = get_and_print_line in_ch in
      Unix.sleep 2
    done
  with
  | End_of_file ->
     let () = Printf.fprintf stderr "I reached the End_of_file and stops here.\n%!" in
     let () = Unix.sleep 4 in
     let () = Printf.printf "list;\n%!" in
     let () = Unix.sleep 4 in
     let () = close_out stdout in
     let () = Printf.fprintf stderr "I closed stdout.\n%!" in
     let () = Unix.sleep 8 in
     let () = Printf.fprintf stderr "I try to output something.\n%!" in
     print_list ()
     



let _ = Arg.parse [] read_and_output "Please read the code!\n%!"


					 
