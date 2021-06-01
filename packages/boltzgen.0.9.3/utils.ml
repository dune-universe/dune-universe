

let str_of_cmd cmd args=
  let open Unix in
  let in_read,in_write = pipe ~cloexec:true () in
  let std_read,std_write = pipe ~cloexec:true ()
  and err_read,err_write = pipe ~cloexec:true () in
  let pid = create_process cmd args in_read std_write err_write in
  close in_write;
  let buff_std = Buffer.create 50
  and buff_err = Buffer.create 50 in
  let lo = ref [std_read; err_read] in
  let buff = Bytes.create 2048 in
  while !lo <> [] do
    Printf.printf "enter: %i" (List.length !lo);
    print_newline ();
    let l,_,_ = select !lo [] [] (-1.0) in
    Printf.printf "select: %i" (List.length l);
    List.iter (fun f ->
                     let n2 = Unix.read f buff 0 2048 in
                     Printf.printf "nr: %i\n" n2;
                     print_endline (Bytes.sub_string buff 0 n2);
                     if n2 <= 0 then (
                       print_endline "test";
                       lo := List.filter (fun x -> x<>f) !lo
                     )
                     else Buffer.add_subbytes (if f=std_read then buff_std else buff_err) buff 0 n2
                     ) l;
    (*let _ = waitpid [WNOHANG] pid in ()*)
  done;
  let r = waitpid [] pid in
  (r,Buffer.contents buff_std, Buffer.contents buff_err)
;;


(*let _ = str_of_cmd "/usr/bin/ls" [|"/usr/bin/ls"|];;*)

let cmd process arg =
  let (outp,inp,errp) = Unix.open_process_args_full process arg [||] in
  let outstr = Buffer.create 2048
  and errstr = Buffer.create 2048 in
  (try while true do
    let line = input_line outp in
    print_endline line;
    Buffer.add_string outstr line
  done with
    End_of_file -> ());
  (try while true do
         let line = input_line errp in
         print_endline line;
         Buffer.add_string errstr line
       done with
     End_of_file -> ());
  let pstat = Unix.close_process_full (outp,inp,errp) in
    match pstat with
      Unix.WEXITED x -> (x, (Buffer.contents outstr), (Buffer.contents errstr))
    | _ -> failwith "pb in out"


let _ =
  let (i,o,e) = cmd "/bin/ping" [|"/bin/ping";"google.com"|] in
  Printf.printf "r:%i out:'%s' err:'%s'" i o e
