open Format

let fname = "OCaml.ml"
let data = "Uploaded by OCaml!"

let bt =
  if Array.length Sys.argv < 2 then begin
    printf "%s <bluetooth addr>\n" Sys.argv.(0);
    exit 1;
  end
  else Sys.argv.(1)

let () =
  let conn = Mindstorm.NXT.connect_bluetooth bt in

  let fh = Mindstorm.NXT.open_out conn (`File (String.length data)) fname in
  let w = Mindstorm.NXT.output fh data 0 (String.length data) in
  printf "Wrote %i bytes to %S.\n%!" w fname;
  Mindstorm.NXT.close_out fh;

  let fh = Mindstorm.NXT.open_in conn fname in
  let len = Mindstorm.NXT.in_channel_length fh in
  let s = Bytes.create len in
  let r = Mindstorm.NXT.input fh s 0 len in
  printf "Contents (%i bytes): %S\n%!" r (Bytes.unsafe_to_string s);
  Mindstorm.NXT.close_in fh;

  printf "@[<2>Files on the brick: ";
  Mindstorm.NXT.Find.iter conn "*.*"  ~f:begin fun name _ ->
    printf "%S  @," name;
  end;
  printf "@]\n%!";

  Mindstorm.NXT.remove conn fname
