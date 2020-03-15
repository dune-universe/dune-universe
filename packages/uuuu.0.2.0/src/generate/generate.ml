#use "topfind";;
#require "re";;

let re =
  let open Re in
  compile (seq [ str "8859-"; group (rep1 digit); str ".TXT" ])

let ( / ) = Filename.concat
let invalid_arg fmt = Format.kasprintf invalid_arg fmt

let make_rule database name =
  let txt_file = name in
  let number = Re.(Group.get (exec re name) 1) |> int_of_string in
  let ml_file = Format.asprintf "ISO_8859_%d.ml" number in

  Format.asprintf
    "(rule \
      (targets %s)
      (deps (:gen ../gen/generate.exe) %s)
      (action (run %%{gen} %s %s)))"
    ml_file (database / txt_file) (database / txt_file) ml_file

let error () =
  invalid_arg "Invalid argument, expected folder database and output file: \
               %s --databases <folder> -o <output>" Sys.argv.(1)

let () =
  let database, output =
    try
      match Sys.argv with
      | [| _; "--databases"; folder; "-o"; output; |] -> folder, output
      | _ -> error ()
    with _ -> error () in

  let files = List.sort String.compare (Array.to_list (Sys.readdir database)) in
  let out = open_out output |> Format.formatter_of_out_channel in

  List.map (make_rule database) files |> List.iter (Format.fprintf out "%s\n\n%!")

