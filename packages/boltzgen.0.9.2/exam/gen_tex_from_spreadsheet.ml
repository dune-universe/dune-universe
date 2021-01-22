open Easy_xlsx
open Printf

let place_holder = [|
    Str.regexp "£A£";
    Str.regexp "£B£";
    Str.regexp "£C£";
    Str.regexp "£D£";
    Str.regexp "£E£";
    Str.regexp "£F£";
    Str.regexp "£G£";
    Str.regexp "£H£";
    Str.regexp "£I£";
    Str.regexp "£J£";
    Str.regexp "£K£";
    Str.regexp "£L£";
    Str.regexp "£M£";
    Str.regexp "£N£";
    Str.regexp "£O£";
    Str.regexp "£P£";
    Str.regexp "£Q£";
    Str.regexp "£R£";
    Str.regexp "£S£";
    Str.regexp "£T£";
    Str.regexp "£U£";
    Str.regexp "£V£";
    Str.regexp "£W£";
    Str.regexp "£X£";
    Str.regexp "£Y£";
    Str.regexp "£Z£"|]


let copy_content f fn dict =
  let fi = open_in fn in
  let fo = open_out f in
  (try while true do
        let line = input_line fi in
        let _,line2 = Array.fold_left (fun (i,str) reg ->
                        match dict i with
                          None -> (i+1,str)
                        | Some tem -> (i+1,Str.global_replace reg tem str)
                      ) (0,line) place_holder in
        output_string fo ((line2)^"\n")
      done with
    End_of_file -> ());
  close_in fi;
  close_out fo

let send_mail logine =
  (*echo "test" | mutt -s "test Email" -- benoit.barbot@u-pec.fr*)
()

let load_line template line=
  let lac = line
  |> List.map (fun x -> match x with
                 Value.String s when s <> "" -> Some s
               | _ -> None )
  |> Array.of_list in
  match lac.(0) with
    Some login ->
     print_endline ("Generating : "^login);
     let out = login^".tex" in
     copy_content out template (fun i -> if i<Array.length lac then lac.(i) else None);
     ignore (Sys.command ("pdflatex "^out))

  |_ ->  ()


       
let load input template =
  read_file input
  |> List.iter (fun sheet ->
         let name = Easy_xlsx.name sheet in
         Printf.printf "Got sheet: %s\n" name;
         if name = "export" then
           match rows sheet with
             [] -> ()
           | _ :: sheet_content ->
              List.iter (load_line template) sheet_content
       )


let _ =
  load Sys.argv.(1) Sys.argv.(2);
