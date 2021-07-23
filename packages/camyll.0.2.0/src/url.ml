let relativize ~src ~dest =
  let chop_common_prefix url1 url2 =
    let rec loop url1 url2 = match url1, url2 with
      | x :: xs, y :: ys when x = y -> loop xs ys
      | url1, url2 -> url1, url2
    in
    loop url1 url2
  in
  if String.length dest > 0 && String.get dest 0 = '/' then
    let src_dir =
      (* chop the filename off the end of the source path *)
      match List.rev (String.split_on_char '/' src) with
      | _ :: xs -> List.rev xs
      | [] -> failwith "Unreachable"
    in
    let dest_file, dest_dir =
      (* chop the filename off the end of the dest path *)
      match List.rev (String.split_on_char '/' dest) with
      | x :: xs -> x, List.rev xs
      | [] -> failwith "Unreachable"
    in
    let src_dir, dest_dir = chop_common_prefix src_dir dest_dir in
    let url =
      [dest_file]
      |> ((@) dest_dir)
      |> List.rev_append (List.init (List.length src_dir) (Fun.const ".."))
      |> String.concat "/"
    in
    if url = "" then
      "./"
    else
      url
  else
    dest
