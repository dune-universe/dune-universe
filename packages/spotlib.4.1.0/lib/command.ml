open Unix
open Base

module Stream = SpotStream

let get_lines s =
  let len = String.length s in
  let rec f pos = 
    if pos >= len then [], "" 
    else
      match 
        Xstring.index_from_to s pos (len-1) '\n'
      with
      | None -> [], String.sub s pos (len-pos)
      | Some pos' ->
          let lines, rem = f (pos'+1) in
          String.sub s pos (pos'+1-pos) :: lines, rem
  in
  f 0

[%%TEST
 let get_lines_ =
   let chr () = match Random.int 24 with
     | 0 -> '\n'
     | n -> Char.chr (Char.code 'a' + n - 1)
   in
   let str () =
     let len = Random.int 10000 in
     let s = Bytes.create len in
     for i = 0 to len - 1 do
       Bytes.unsafe_set s i (chr ())
     done;
     Bytes.to_string s
   in
   for _i = 0 to 1000 do
     let s = str () in
     let lns, rem = get_lines s in
     assert (String.concat "" lns ^ rem = s)
   done
]

let liner ?(cut_threshold=10000) () =
  let buf = Buffer.create 1024 in
  fun eof s ->
    let lines, rem = get_lines s in
    assert (String.concat "" lines ^ rem = s);
    match lines with
    | [] -> (* no new line in s *)
        Buffer.add_string buf rem;
        if eof || Buffer.length buf > cut_threshold then begin
          let s = Buffer.contents buf in
          Buffer.clear buf;
          if s <> "" then [s] else []
        end else []
    | l::ls ->
        let l = Buffer.contents buf ^ l in
        Buffer.clear buf;
        if eof then
          if rem <> "" then l::ls@[rem] else l::ls
        else begin
          Buffer.add_string buf rem;
          l::ls
        end

[%%TEST
 let liner_ =
   let open List in
   let chr () = match Random.int 24 with
     | 0 -> '\n'
     | n -> Char.chr (Char.code 'a' + n - 1)
   in
   let str () =
     let len = Random.int 10000 in
     let s = Bytes.create len in
     for i = 0 to len - 1 do
       Bytes.unsafe_set s i (chr ())
     done;
     Bytes.to_string s
   in
   let ln = liner () in
   let rec loop rev_in rev_out =
     let s = str () in
     match Random.int 100 with
     | 0 -> (rev (s::rev_in)), rev (rev_append (ln true s) rev_out)
     | _ -> loop (s::rev_in) (rev_append (ln false s) rev_out)
   in
   for _i = 0 to 100 do
     let ins, outs = loop [] [] in
     if String.concat "" ins <> String.concat "" outs then begin
       Format.eprintf "ins : %s@." (String.concat ",\n" ins);
       Format.eprintf "outs: %s@." (String.concat ",\n" outs);
       assert false
     end
   done
]
          
let multi_stream_read xs = 
  let buflen = 4096 in
  let buf = Bytes.create buflen in
  let open Stream in
  let rec f xs = lazy begin
    if xs = [] then Null
    else
      let fds = List.map (fun (fd, _, _) -> fd) xs in 
      let readables, _, _ =
        let rec f () =
          try select fds [] [](*?*) (-1.0)(*?*) with
          | Unix_error (EINTR, _, _) -> f ()
        in
        f ()
      in
      let rev_tks, rev_xs =
        List.fold_left (fun (rev_tks, rev_xs) (fd, fattr, ftknize as x) ->
          if not (List.mem fd readables) then
            rev_tks, x::rev_xs
          else 
            match read fd buf 0 buflen with
            | exception Unix_error ((EAGAIN | EINTR | EWOULDBLOCK), _, _) -> 
                rev_tks, 
                x :: rev_xs
            | exception Unix_error (EPIPE, _, _) -> 
                (* eof in Windows pipe *)
                (* Exn.try_ignore Unix.close fd; *)
                let tkns = ftknize true "" in
                fattr `EOF
                :: List.(rev_append 
                           (map (fun x -> fattr (`Read x)) tkns)
                           rev_tks), 
                rev_xs
            | 0 -> 
                (* eof in Windows pipe *)
                (* Exn.try_ignore Unix.close fd; *)
                let tkns = ftknize true "" in
                fattr `EOF
                :: List.(rev_append 
                           (map (fun x -> fattr (`Read x)) tkns)
                           rev_tks), 
                rev_xs
            | len -> 
                let tkns = ftknize false & Bytes.sub_string buf 0 len in
                List.(rev_append 
                        (map (fun x -> fattr (`Read x)) tkns)
                        rev_tks), 
                x :: rev_xs
            | exception (Unix_error _ as e) -> 
                (* error *)
                (* Exn.try_ignore Unix.close fd;  *)
                let tkns = ftknize true "" in
                fattr (`Error e) 
                :: List.(rev_append
                           (map (fun x -> fattr (`Read x)) tkns)
                           rev_tks),
                rev_xs)
          ([], []) xs
      in
      let s = f & List.rev rev_xs in
      let rev_append rev s = 
        let rec f s = function
          | [] -> s
          | t::ts -> f (Lazy.from_val (Cons (t, s))) ts
        in
        f s rev
      in
      Lazy.force & rev_append rev_tks s
  end
  in
  f xs

type output =
  [ `Err of [ `EOF | `Error of exn | `Read of string ]
  | `Out of [ `EOF | `Error of exn | `Read of string ]
  ]

(* Unix.open_process_* only takes concatenated command string... *)
let escape = Filename.quote

(* In Win32, the following works:
open Unix
let () =
  let ic = open_process_in "\"\"ocamlc\" \"-version\"\"" in
  let rec f () =
    try
      let s = input_line ic in
      prerr_endline s;
      f ()
    with
    | End_of_file -> ()
  in
  f ()
*)
let escape_for_shell ss =
  let s = String.concat " " & List.map escape ss in
  match Sys.os_type with
  | "Win32" -> "\"" ^ s ^ "\""
  | _ -> s

let raw_exec ?env cmd =
  let env = Option.default env environment in
  let ic, oc, ec = open_process_full cmd env in
  close_out oc;
  let ifd = descr_of_in_channel ic in
  let efd = descr_of_in_channel ec in
  (multi_stream_read 
    [ ifd, (fun x -> `Out x), liner ()
    ; efd, (fun x -> `Err x), liner () ],
   fun () -> close_process_full (ic, oc, ec) (* close failure of oc is ignored *))

let fail = function
  | WEXITED n   -> Exn.failwithf "process exited with id %d" n
  | WSIGNALED n -> Exn.failwithf "process killed by signal %d" n
  | WSTOPPED n  -> Exn.failwithf "process stopped by signal %d" n

module Ver1 = struct    

  let shell_exec ?env cmd f exit =
    let s, closer = raw_exec ?env cmd in
    let res = Exn.catch f s in
    let w = closer () in
    exit (res, w)
  
  let exec ?env cmd =
    let s = escape_for_shell cmd in
    shell_exec ?env s
  
  let must_exit_with n (res, w) = 
    match w with
    | WEXITED m when n = m ->
        begin match res with
        | Ok v -> v
        | Error (`Exn e) -> raise e
        end
    | _ -> fail w
  
  let force_lines s = 
    Stream.filter_map (function
      | `Out (`Read s) | `Err (`Read s) -> Some s
      | _ -> None) s
    |> Stream.to_list
  
  let force_stdout s = 
    Stream.filter_map (function
      | `Out (`Read s) -> Some s
      | _ -> None) s
    |> Stream.to_list
  
  let force_stderr s = 
    Stream.filter_map (function
      | `Err (`Read s) -> Some s
      | _ -> None) s
    |> Stream.to_list
  
  let print s = 
    Stream.iter (function
      | `Out (`Read s) -> print_string s; Stdlib.(flush stdout)
      | `Err (`Read s) -> prerr_string s; Stdlib.(flush stderr)
      | `Out (`Error e) -> Printf.eprintf "Exception at reading stdout: %s\n%!" (Exn.to_string e)
      | `Err (`Error e) -> Printf.eprintf "Exception at reading stderr: %s\n%!" (Exn.to_string e)
      | _ -> ()) s
end

module Ver2 = struct
    
  type 'a t = { a: 'a
              ; closer: (unit ->  process_status)
              ; cmd : string
              }
  
  let shell ?env cmd =
    let str, closer = raw_exec ?env cmd in
    { a = str; closer; cmd }

  let exec ?env cmd =
    let s = escape_for_shell cmd in
    shell ?env s
  
  let command at = at.cmd
  let wait at = let ps = at.closer () in at.a, ps
  let map f at = { at with a = f at.a }
  let print =
    let f = Stream.map (fun x ->
      begin match x with
      | `Out (`Read s) -> print_string s; Stdlib.(flush stdout)
      | `Err (`Read s) -> prerr_string s; Stdlib.(flush stderr)
      | `Out (`Error e) -> Printf.eprintf "Exception at reading stdout: %s\n%!" (Exn.to_string e)
      | `Err (`Error e) -> Printf.eprintf "Exception at reading stderr: %s\n%!" (Exn.to_string e)
      | `Out `EOF | `Err `EOF -> ()
      end;
      x)
    in
    map f

  let gen_lines g =
    let f = Stream.(to_list *< filter_map g)
    in
    map f

  let lines = gen_lines (function
    | `Out (`Read s) | `Err (`Read s) -> Some s
    | `Out (`Error e) -> Printf.eprintf "Exception at reading stdout: %s\n%!" (Exn.to_string e); None
    | `Err (`Error e) -> Printf.eprintf "Exception at reading stderr: %s\n%!" (Exn.to_string e); None
    | _ -> None)

  let stdout = gen_lines (function
    | `Out (`Read s) -> Some s
    | `Out (`Error e) -> Printf.eprintf "Exception at reading stdout: %s\n%!" (Exn.to_string e); None
    | `Err (`Error e) -> Printf.eprintf "Exception at reading stderr: %s\n%!" (Exn.to_string e); None
    | _ -> None)

  let stderr = gen_lines (function
    | `Err (`Read s) -> Some s
    | `Out (`Error e) -> Printf.eprintf "Exception at reading stdout: %s\n%!" (Exn.to_string e); None
    | `Err (`Error e) -> Printf.eprintf "Exception at reading stderr: %s\n%!" (Exn.to_string e); None
    | _ -> None)


  let iter f at = { at with a = Stream.iter f at.a }

  let fold f init at = { at with a = Stream.fold_left' f init at.a }

  let void at = { at with a = () }
    
  let must_exit_with n (res, w) = 
    match w with
    | WEXITED m when n = m -> res
    | _ -> fail w
end
  
include Ver2

