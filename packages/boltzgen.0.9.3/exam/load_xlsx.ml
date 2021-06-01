open Question_type
open Easy_xlsx
open Type_lib


let remove_strange = Str.regexp_string "&#10;"
let cstr s =
  Str.global_replace remove_strange "\n" s


let load_question row =
  let open Value in
  match row with
  | _ :: String s :: _ when s="" -> None
  | String s :: _ when String.length s>0 && s.[0] = '#' -> print_endline ("Ignore question:"^s); None 
  | String t :: String "type" :: String answer :: _ ->
     Some (mkdeftype t answer)
  | String t :: String s:: String answer :: String a1 :: String a2 :: String a3 :: String a4 ::_ when s ="qcm" ->
     Some ( mkqcm (cstr t) [cstr a1; cstr a2; cstr a3; cstr a4] (qcm_of_string answer))
  | String t :: String s:: String answer :: Number te :: _ ->
     let test_effort = int_of_float te in
     Some (mkfun ~test_effort (cstr t) s (cstr answer) [])
  | String t :: String s:: String answer :: q when List.for_all (function String _ -> true |_-> false) q ->
     let q2 = q
              |> List.map (function String s -> cstr s |_ -> assert false)
              |> List.filter (fun s -> String.trim s <> "") in
     Some (mkfun (cstr t) (cstr s) (cstr answer) q2)
 
  | String s :: _ -> print_endline ("fail to parse :"^s); None
  | _ -> None


let load input output =
  let qbank =
    let digest = Digest.file input in
    let qb = read_file input
    |> List.fold_left (fun acc sheet ->
           Printf.printf "Got sheet: %s\n" (Easy_xlsx.name sheet);
           match rows sheet with
             [] -> acc
           | _ :: sheet_content ->
              List.fold_left (fun acc row ->
                  match load_question row with
                    None -> acc
                  | Some Type qu -> 
                     print_endline ("New question, type : Type");
                     (Type qu) :: acc
                  | Some Value qu ->
                     print_endline ("New question, type : "^ (Type.string_of_compo qu.rtype));
                     (Value qu) :: acc
                ) acc sheet_content
          
         ) []
       in
       print_endline ("QB:"^(Digest.to_hex digest));
       { questions = Array.of_list qb; digest; shuffle = None}  in
  save_qbank output qbank

let _ =
  load Sys.argv.(1) "qbank_from_spreadsheet";
