(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Jun Furuse, projet Cristal, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1999,2000,2001,2002,2001,2002                            *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)
open Images
open OImages

(*
let _ =
  Bitmap.maximum_live := 15000000; (* 60MB *)
  Bitmap.maximum_block_size := !Bitmap.maximum_live / 16;

*)


let _ = prerr_endline "init done"

module D = Display
open D

(* open Gc *)
open Unix
open LargeFile
open GMain

open Livmisc
open Gui
open Tout

exception Skipped

let cwd = Unix.getcwd ()
let home = Sys.getenv "HOME"

let convert_file file = 
  let b = Buffer.create (String.length file) in

  let rec loop file =
    let dir = Filename.dirname file in
    let base = Filename.basename file in
    begin match dir with
    | "." -> Buffer.add_string b dir
    | "/" -> ()
    | _ -> loop dir
    end;
    Buffer.add_char b '/';
    Buffer.add_string b (try Glib.Convert.locale_to_utf8 base with _ -> base)
  in
  loop file;
  Buffer.contents b


let base_filters = ref ([] : D.filter list)

let _ =
  let files = ref [] in
  let random = ref false in
  let dirrandom = ref false in
  let dirsample = ref false in
  let size = ref false in

  Random.init (Pervasives.truncate (Unix.time ()));
  Arg.parse 
    [
      "-random", Arg.Unit (fun () -> random := true), ": random mode";
      "-dirrandom", Arg.Unit (fun () -> dirrandom := true), ": random per directory mode";
      "-dirsample", Arg.Unit (fun () -> dirsample := true), ": random per directory sample mode";
      "-wait", Arg.Float (fun sec -> Tout.wait := sec), "sec : wait sec";
      "-root", Arg.String (function
	  "center" -> D.root_mode := `CENTER
	| "random" -> D.root_mode := `RANDOM
	| _ -> raise (Failure "root mode")), ": on root [center|random]";
(*
      "-transition", Arg.String (function
	  "myst" -> D.transition := `MYST
	| "transparent" -> D.transition := `TRANSPARENT
	| _ -> raise (Failure "transition")), ": transition [myst|transparent]";
      "-transparentborder", Arg.Unit (fun () ->
	base_filters := `TRANSPARENT_BORDER :: !base_filters),
      ": transparent border filter";
*)
      "-size", Arg.String (fun s ->
	match Mstring.split_str (function 'x' -> true | _ -> false) s with
	  [w;h] -> 
	    size := true; 
	    base_filters := `SIZE (int_of_string w, int_of_string h,`NOASPECT) :: !base_filters
  	| _ -> raise (Failure "size")), ": size [w]x[h]";
      "-atleast", Arg.String (fun s ->
	match Mstring.split_str (function 'x' -> true | _ -> false) s with
	  [w;h] -> 
	    size := true; 
	    base_filters := `SIZE (int_of_string w, int_of_string h,`ATLEAST) :: !base_filters
  	| _ -> raise (Failure "zoom")), ": zoom [w]x[h]";
      "-atmost", Arg.String (fun s ->
	match Mstring.split_str (function 'x' -> true | _ -> false) s with
	  [w;h] -> 
	    size := true; 
	    base_filters := `SIZE (int_of_string w, int_of_string h,`ATMOST) :: !base_filters
  	| _ -> raise (Failure "zoom")), ": zoom [w]x[h]";

    ]  
    (fun s -> files := s :: !files)
    "liv files";

  let files =
    let fs = ref [] in
    List.iter (fun f ->
      try
	let st = stat f in
	match st.st_kind with
	| S_DIR ->
	    Scandir.scan_dir (fun f -> 
	      try 
		ignore (guess_extension (snd (Livmisc.get_extension f)));
		fs := f :: !fs;
	      with _e -> (* prerr_endline ((f^": "^ Printexc.to_string e)) *) ()) f
	| _ -> fs := f :: !fs
      with
      | _ -> prerr_endline ("ERROR: " ^ f)) !files;
    Array.of_list !fs 
  in

  if not !size then
    base_filters := `SIZE (fst root_size, snd root_size, `ATMOST) 
                         :: !base_filters;
  base_filters := List.rev !base_filters;
  
  let cur = ref (-1) in
  let curpath = ref "" in

  let disp_cur = ref (-1) in

  let random_array ary = 
    let num = Array.length ary in
    for i = 0 to num - 1 do
      let tmp = ary.(i) in
      let pos = Random.int num in
      ary.(i) <- ary.(pos);
      ary.(pos) <- tmp
    done
  in

  if !dirsample then begin
    let tbl = Hashtbl.create 17 in
    let dirs = ref [] in
    let num_files = Array.length files in
    for i = 0 to num_files - 1 do
      let dirname = Filename.dirname files.(i) in
      Hashtbl.add tbl dirname files.(i);
      if not (List.mem dirname !dirs) then dirs := dirname :: !dirs
    done;
    let dirsarray = Array.of_list !dirs in
    random_array dirsarray;
    let pos = ref 0 in
    let subpos = ref 0 in
    let subfiles = Array.init (Array.length dirsarray) (fun a ->
      let ary = Array.of_list (Hashtbl.find_all tbl dirsarray.(a)) in
      random_array ary; ary)
    in
    while !pos < Array.length files do
      for i = 0 to Array.length dirsarray - 1 do
	if !subpos < Array.length subfiles.(i) then begin
	  files.(!pos) <- subfiles.(i).(!subpos);
	  incr pos
	end
      done;
      incr subpos
    done 
  end else
  if !dirrandom then begin
    let tbl = Hashtbl.create 17 in
    let dirs = ref [] in
    let num_files = Array.length files in
    for i = 0 to num_files - 1 do
      let dirname = Filename.dirname files.(i) in
      Hashtbl.add tbl dirname files.(i);
      if not (List.mem dirname !dirs) then dirs := dirname :: !dirs
    done;
    let dirsarray = Array.of_list !dirs in
    random_array dirsarray;
    let pos = ref 0 in
    for i = 0 to Array.length dirsarray - 1 do
      let dirfiles = Array.of_list 
	  (List.sort compare (Hashtbl.find_all tbl dirsarray.(i))) in
      if !random then begin
	random_array dirfiles
      end;
      for j = 0 to Array.length dirfiles - 1 do
	files.(!pos) <- dirfiles.(j);
	incr pos
      done
    done
  end else if !random then random_array files;

  infowindow#show ();

  imglist#freeze ();
  Array.iter (fun file -> 
    ignore (imglist#append [convert_file file]))
    files;
  imglist#thaw ();

  let cache = Cache.create 5 in

  let _rename pos newname =
    let oldname = files.(pos) in
    let xvname s = Filename.dirname s ^ "/.xvpics/" ^ Filename.basename s in
    let oldxvname = xvname oldname in
    let newxvname = xvname newname in
    let gthumbname s = 
      let abs = 
	if s = "" then "" else 
	if s.[0] = '/' then s
	else Filename.concat cwd s
      in
      (Filename.concat (Filename.concat home ".gnome2/gthumb/comments") abs)
	^ ".xml"
    in
    let oldgthumbname = gthumbname oldname in
    let newgthumbname = gthumbname newname in
    imglist#set_cell ~text: (convert_file newname) pos 0;
    let command s = Sys.command s in
    if Filename.dirname newname <> Filename.dirname oldname then begin
      ignore (command 
		(Printf.sprintf "mkdir -p %s" (Filename.dirname newname)));
    end;
    prerr_endline (Printf.sprintf "%s => %s" oldname newname); 
    ignore (command 
	      (Printf.sprintf "yes no | mv -i \"%s\" \"%s\"" oldname newname));
    if Sys.file_exists oldxvname then begin
      ignore (command 
		(Printf.sprintf "mkdir -p %s" (Filename.dirname newxvname)));
	ignore (command 
		  (Printf.sprintf "yes no | mv -i \"%s\" \"%s\"" oldxvname newxvname))
    end;
    if Sys.file_exists oldgthumbname then begin
      ignore (command 
		(Printf.sprintf "mkdir -p %s" (Filename.dirname newgthumbname)));
      ignore (command 
		(Printf.sprintf "yes no | mv -i \"%s\" \"%s\"" oldgthumbname newgthumbname))
    end;
    files.(pos) <- newname;
    Cache.rename cache oldname newname
  in

  let image_id = ref 0 in

  let display_image reload file =
    (* prerr_endline file; *)
    remove_timeout ();

    let load_image () =
      prog#map (); 
      prog#set_fraction 0.01; 
      prog#set_format_string ("loading...");
      let image = 
	try
  	  match tag (OImages.load file 
  		       [Load_Progress prog#set_fraction]) with
  	  | Rgb24 i -> i
	  | Rgba32 i -> i#to_rgb24
  	  | Index8 i -> i#to_rgb24
  	  | Index16 i -> i#to_rgb24
  	  | _ -> raise (Failure "not supported")
	with 
	| e -> prerr_endline (Printexc.to_string e); raise e
      in
      prog#set_fraction 1.0; sync ();
      image
    in

    let id, image =
      try
      	if not reload then begin
      	  Cache.find cache file
	end else raise Not_found
      with
	Not_found ->
	  let im = load_image () in
	  incr image_id;
	  !image_id, im
    in
    Cache.add cache file (id, image) (fun _ -> ());
    
    prog#set_fraction 0.01;
    display id image !base_filters; (* this cause lots of gc *)

    window#set_title (convert_file file);
    
    disp_cur := !cur;
    curpath := file;
  in

  let display_image reload file =
    try 
      display_image reload file 
    with Wrong_file_type | Wrong_image_type ->
      try
	prerr_endline "guess type";
	let typ =
	  let typ = Livshtype.guess file in
	  match typ with
	  | Livshtype.ContentType x ->
	      begin match
		Mstring.split_str (function '/' -> true | _ -> false) x
	      with
	      | [mj;mn] -> mj,mn
      	      | _ -> assert false
	      end
	  | Livshtype.ContentEncoding x ->
	      "encoding", x
	  | Livshtype.Special m ->
	      "special",m
	in
	prerr_endline (fst typ ^ "/" ^ snd typ);  
	match typ with
	| _ -> raise Wrong_file_type
      with
      | _ -> ()
  in

  let _filter_toggle opt = 
	if List.mem opt !base_filters then
	  base_filters :=
	     List.fold_right (fun x st ->
	       if x = opt then st
	       else x :: st) !base_filters []
	else
	  base_filters := !base_filters @ [opt]
  in

  let display_current reload =
    let f = 
      if !cur >= 0 && !cur < Array.length files then begin
    	imglist#unselect_all ();
    	imglist#select !cur 0;
    	if imglist#row_is_visible !cur <> `FULL then begin
	  imglist#moveto ~row_align: 0.5 ~col_align: 0.0 !cur 0
    	end;
      	files.(!cur)
      end else !curpath
    in

      display_image reload f;

    ()
  in


  let rec next mode =
    if !cur >= 0 then begin
      let cur' = 
  	if !cur >= Array.length files - 1 then 0 else !cur + 1
      in
      if !cur = cur' then ()
      else begin
  	cur := cur';
  	try
  	  display_current false;
      	with
      	| Sys_error s ->
  	    prerr_endline s;
  	    next mode
        | Wrong_file_type | Wrong_image_type -> next mode
      end
    end
  in

  let rec prev mode =
    if !cur >= 0 then begin
      let cur' =
      	if !cur = 0 then Array.length files - 1 else !cur - 1
      in
      if !cur = cur' then ()
      else begin
      	cur := cur';
      	try
  	  display_current false
      	with
      	| Sys_error s ->
  	    prerr_endline s;
  	    prev mode
      	| Skipped -> prev mode
      	| Wrong_file_type | Wrong_image_type -> prev mode
      end
    end
  in

  let bind () =
    let callback = fun ev ->
      begin match GdkEvent.Key.string ev with
(*
      | "E" -> 
	  filter_toggle `ENHANCE;
	  display_current true

*)
(*
      | "N" -> 
	  filter_toggle `NORMALIZE;
	  display_current true
*)
	    
      |	"l" -> display_current true

      | " " | "n" | "f" -> next None
      | "p" | "b" -> prev None
      | "q" -> Main.quit ()
(*
      | "v" -> 
	(* liv visual shell *)
  	  let rec func = fun file typ ->
	    match typ with
	    | "image", _ -> 
    	      	display_image false file
(*
            | "special", "dir" -> 
                new Livsh.livsh file func; ()
*)
	    | _ -> Gdk.X.beep ()
  	  in
	  (* where we should display ? *)
	  let dirname = 
	    if Array.length files = 0 then Unix.getcwd ()
	    else Filename.dirname files.(!cur) 
	  in
	  let dirname =
	    if Filename.is_relative dirname then begin
 	      let cwd = Unix.getcwd () in
	      Filename.concat cwd dirname
	    end else dirname
	  in
	  ignore (new Livsh.livsh dirname func)
*)
      | _ -> () 
      end; false
    in
    ignore (window#event#connect#key_press ~callback: callback);
    ignore (infowindow#event#connect#key_press ~callback: callback);

    ignore (imglist#connect#select_row ~callback: (fun ~row ~column:_ ~event:_ ->
      if !cur <> row then begin
      	cur := row;
      	display_image false files.(!cur)
      end))
  in

  bind ();

  Tout.hook_next := next;

  window#show ();

  let starter = ref None in

  starter := Some (window#event#connect#configure ~callback: (fun _ev ->
    may window#misc#disconnect !starter;
    if Array.length files <> 0 then begin
      cur := 0;
      prog#unmap ();
      display_current false
    end else begin
      try
	display_image false (Pathfind.find [ "~/.liv"; 
					     "/usr/lib/liv"; 
					     "/usr/local/lib/liv";
					     "." ] "liv.jpg")
      with
      | _ -> ()
    end; false));

(*
  let release _ = prerr_endline "freed string!" in
  let test () =
    let f () =
      let string = String.create 3000 in
      Gc.finalise release string;
      let buf = Gpointer.region_of_string string in
      ignore (GdkPixbuf.from_data ~width: 100 ~height: 10
		~bits: 8 ~rowstride:300 ~has_alpha: false buf);
      ()
    in
    for i = 0 to 100 do f () done
  in
  test ();
*)

  Main.main ()
