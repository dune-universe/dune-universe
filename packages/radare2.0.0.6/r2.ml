type r2 =
  {pid : int; read_from : Unix.file_descr; write_to : Unix.file_descr}

exception Stop_read

let read_result read_from =
  let b = Buffer.create 1 in
  let buf = Bytes.create 1 in
  begin
    try
      while true do
        Unix.read read_from buf 0 1 |> ignore;
        if buf = (Bytes.of_string "\x00") then raise Stop_read
        else Buffer.add_bytes b buf
      done
    with Stop_read -> ()
  end;
  Buffer.to_bytes b

let send_command {write_to; read_from; _} cmd =
  let c = Printf.sprintf "%s\n" cmd in
  ignore (Unix.write_substring write_to c 0 (String.length c));
  read_result read_from |> Bytes.trim |> Bytes.to_string

let command ~r2 cmd = send_command r2 cmd

let parse_json s =
  (Yojson.Safe.from_string s :> Yojson.t)

let command_json ~r2 cmd =
  try
    send_command r2 cmd |> parse_json
  with
    Yojson.Json_error _ ->
    raise (Invalid_argument "Output wasn't JSON parsable, \
                             make sure you used /j")

module Version : sig
  val supported : unit Lazy.t
end
= struct
  let system_error msg =
    invalid_arg ("Failed to run radare2: " ^ Unix.error_message msg)

  let try_finally f x ~finally =
    try let r = f x in finally x; r
    with exn -> finally x; raise exn

  let read_version () =
    match Unix.open_process_in "r2 -qv" with
    | exception Unix.Unix_error (msg,_,_) -> system_error msg
    | output -> try_finally input_line output
                  ~finally:close_in

  let extract_number str pos len =
    try int_of_string (String.sub str pos len)
    with Failure _ -> invalid_arg "invalid version format"

  let parse ver =
    let len = String.length ver in
    match String.index ver '.' with
    | exception Not_found -> extract_number ver 0 len,0
    | pos ->
      extract_number ver 0 pos,
      if pos = len - 1 then 0
      else match String.index_from ver (pos+1) '.' with
        | exception Not_found ->
          if pos = len - 1 then 0
          else extract_number ver (pos+1) (len-pos-1)
        | dot ->
          extract_number ver (pos+1) (dot-pos-1)

  let supported = lazy begin
    let version = parse @@ read_version () in
    if version < (2,3)
    then invalid_arg "incompatible radare version: please install r2 >= 2.3.0"
  end
end

let open_file f_name =
  let lazy () = Version.supported in
  if not (Sys.file_exists f_name) then
    raise (Invalid_argument "Non-existent file")
  else
    let (ins_r, ins_w),
        (out_r, out_w),
        (_, err_w) = Unix.(pipe (), pipe (), pipe ())
    in
    let args = [|"r2"; "-2"; "-q0"; f_name|] in
    let pid = Unix.create_process "r2" args ins_r out_w err_w in
    (* Get rid of the beginning \x00 *)
    ignore (Unix.read out_r (Bytes.create 1) 0 1);
    {pid; read_from = out_r; write_to = ins_w}

(* Heavy handed but we ensure that r2 is killed *)
let close {pid; _} =
  Unix.kill pid Sys.sigkill;
  Unix.waitpid [] pid |> ignore;
  ()

let with_command ~cmd f_name =
  let r2 = open_file f_name in
  let output = command ~r2 cmd in
  close r2;
  output

let with_command_j ~cmd f_name =
  let r2 = open_file f_name in
  let output = command ~r2 cmd in
  close r2;
  output |> parse_json
