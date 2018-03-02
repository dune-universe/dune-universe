(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            François Pessaux, projet Cristal, INRIA Rocquencourt     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*            Jun Furuse, projet Cristal, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1999-2004,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: gif.ml,v 1.2 2008/06/16 22:35:42 furuse Exp $ *)

open Images
open Index8
open Util

let debug =
  try ignore (Sys.getenv "CAMLIMAGES_DEBUG_GIF"); true with
  | _ -> false

let debug_endline = if debug then prerr_endline else (fun _ -> ())

type in_channel
type out_channel

type screen_info = {
    s_width : int;
    s_height : int;
    s_color_resolution : int;
    s_back_ground_color : int;
    s_colormap : rgb array;
}

type record_type =
  | Undefined
  | Screen_desc
  | Image_desc
  | Extension
  | Terminate

type gif_desc = {
    desc_left : int;
    desc_top : int;
    desc_width : int;
    desc_height : int;
    desc_interlace : bool;
    desc_colormap : Color.rgb array;
}

external dGifOpenFileName : string -> screen_info * in_channel
    = "dGifOpenFileName"
external dGifCloseFile : in_channel -> unit
    = "dGifCloseFile"
external dGifGetRecordType : in_channel -> record_type
    = "dGifGetRecordType"
external dGifGetImageDesc : in_channel -> gif_desc
    = "dGifGetImageDesc"
external dGifGetLine : in_channel -> bytes
    = "dGifGetLine"
external dGifGetExtension : in_channel -> int * string list (* reversed!!! *)
    = "dGifGetExtension"

external eGifOpenFileName : string -> out_channel
    = "eGifOpenFileName"
external eGifCloseFile : out_channel -> unit
    = "eGifCloseFile"
external eGifPutScreenDesc : out_channel -> screen_info -> unit
    = "eGifPutScreenDesc"
external eGifPutImageDesc : out_channel -> gif_desc -> unit
    = "eGifPutImageDesc"
external eGifPutLine : out_channel -> bytes -> unit
    = "eGifPutLine"
external eGifPutExtension : out_channel -> int * string list -> unit
    = "eGifPutExtension"

type gif_extension =
   | GifComment of string list
   | GifGraphics of string list
   | GifPlaintext of string list
   | GifApplication of string list
   | GifOtherExt of int * string list

type gif_frame = {
    frame_left : int;
    frame_top : int;
    frame_bitmap : Index8.t;
    mutable frame_extensions : gif_extension list;
    frame_delay : int;
}

type gif_sequence = {
    screen_width : int;
    screen_height : int;
    screen_colormap : Color.rgb Color.map;
    frames : gif_frame list;
    loops : int;
}

let gif_parse_extension func exts =
  let exts = List.rev exts in
  match func with
  | 0xfe -> GifComment exts
  | 0xf9 -> GifGraphics exts
  | 0x01 -> GifPlaintext exts
  | 0xff -> GifApplication exts
  | _ -> GifOtherExt (func, exts)

let gif_make_extension ext =
  match ext with
  | GifComment exts -> 0xfe, exts
  | GifGraphics exts -> 0xf9, exts
  | GifPlaintext exts -> 0x01, exts
  | GifApplication exts -> 0xff, exts
  | GifOtherExt (func, exts) -> func, exts

open Printf

let load filename opts =
  let prog = Images.load_progress opts in
  let sinfo, ic = dGifOpenFileName filename in
  debug_endline
    (sprintf "Screen = %dx%d ColorRes = %d BG = %d Colors = %d"
       sinfo.s_width sinfo.s_height
       sinfo.s_color_resolution
       sinfo.s_back_ground_color
       (Array.length sinfo.s_colormap));
  let frames = ref [] in
  let current_extensions = ref [] in
  let transparent = ref (-1) in
  let loops = ref 0 in
  let delay = ref 0 in
  let only_the_first_frame = List.mem Load_only_the_first_frame opts in
  try
    while true do
      if only_the_first_frame && !frames <> [] then raise Exit;
      match dGifGetRecordType ic with
      | Terminate -> raise Exit
      | Image_desc ->
         let desc = dGifGetImageDesc ic in
         (* reset progress bar *)
         begin match prog with
         | Some p -> p 0.0
         | None -> ()
         end;

         if debug then
           debug_endline
             (sprintf "Size=%dx%d Colors=%d"
                desc.desc_width desc.desc_height
                (Array.length desc.desc_colormap));

         let img = Index8.create desc.desc_width desc.desc_height in
         img.transparent <- !transparent;
         let cmap = img.colormap in
         cmap.max <- 256;
         cmap.map <-
           (if Array.length desc.desc_colormap = 0
            then sinfo.s_colormap
            else desc.desc_colormap);

         if debug then
           for i = 0 to Array.length cmap.map -1 do
             prerr_string
               (sprintf " %2d: %02xh %02xh %02xh   "
                  i cmap.map.(i).r cmap.map.(i).g cmap.map.(i).b);
             if i mod 4 = 3 then prerr_endline "";
           done;

         (* Interlaced gif encoding
            dst              src
            0   <----------- 0
            8   <----------- 1
            16  <----------- 2
            ... <----------- ...
            ... <----------- x-1
            4   <----------- x
            12  <----------- x+1
            20  <----------- x+2
            ... <----------- ...
            ... <----------- y-1
            2   <----------- y
            6   <----------- y+1
            10  <----------- y+2
            14  <----------- y+3
            ... <----------- ...
            ... <----------- z-1
            1   <----------- z
            3   <----------- z+1
            5   <----------- z+2
            7   <----------- z+3
            9   <----------- z+4
            11  <----------- z+5
            ... <----------- ...
                <----------- image.height - 1
         *)

         let interlace_reader () =
           let lines = ref 0 in
           let rec loop src dest dest_step =
             if dest >= desc.desc_height then src else begin
               let line = dGifGetLine ic in
               Index8.set_scanline img dest line;
               incr lines;
               begin match prog with
               | Some p -> p (float !lines /. float desc.desc_height)
               | None -> ()
               end;
               loop (src + 1) (dest + dest_step) dest_step
             end in
           let src_1 = loop     0 0 8 in
           let src_2 = loop src_1 4 8 in
           let src_3 = loop src_2 2 4 in
           ignore (loop src_3 1 2) in

         let normal_reader () =
           for y = 0 to desc.desc_height - 1 do
             let line = dGifGetLine ic in
             Index8.set_scanline img y line;
             begin match prog with
             | Some p -> p (float (y + 1) /. float desc.desc_height)
             | None -> ()
             end;
           done in

         begin
           try
             if desc.desc_interlace
             then interlace_reader ()
             else normal_reader ()
           with
           | Failure _e -> prerr_endline "Short file";
         end;

         let frame = {
           frame_left = desc.desc_left;
           frame_top = desc.desc_top;
           frame_bitmap = img;
           frame_extensions = !current_extensions;
           frame_delay = !delay;
         } in
         current_extensions := [];
         transparent := (-1);
         delay := 0;
         frames := frame :: !frames
      | Extension ->
         debug_endline "EXTENSION";
         let func, exts = dGifGetExtension ic in
         let ext = gif_parse_extension func exts in
         begin match ext with
         | GifGraphics (str :: _ as exts) ->
            if debug then begin
              prerr_string "GRP: ";
              for i = 0 to String.length str - 1 do
                prerr_string (Printf.sprintf "%02x " (int_of_char str.[i]))
              done
            end;
            List.iter debug_endline exts;
            if String.length str < 4 then raise Exit;
            if int_of_char str.[0] land 0x1 <> 0 then begin
              debug_endline
               (Printf.sprintf "TRANSPARENT %d" (int_of_char str.[3]));
              transparent := int_of_char str.[3]
            end;
            delay := int_of_char str.[1] + int_of_char str.[2] * 256;
         | _ -> ()
         end;
         current_extensions := ext :: !current_extensions
      | Undefined | Screen_desc -> raise (Failure "Gif.load")
    done;
    raise (Failure "impos")
  with
  | Exit ->
      debug_endline "CLOSING";
      dGifCloseFile ic;
      debug_endline "FINISHED";
      { screen_width = sinfo.s_width;
        screen_height = sinfo.s_height;
        screen_colormap = {max = 256; map = sinfo.s_colormap; };
        frames = (List.rev !frames);
        loops = !loops; }
  | Failure e ->
      prerr_endline ("Gif.load Error " ^ e);
      debug_endline "CLOSING";
      dGifCloseFile ic;
      debug_endline "FINISHED";
      { screen_width = sinfo.s_width;
      screen_height = sinfo.s_height;
      screen_colormap = {max = 256; map = sinfo.s_colormap; };
      frames = (List.rev !frames);
      loops = !loops }

let seq_of_gifseq gifseq = {
  seq_width = gifseq.screen_width;
  seq_height = gifseq.screen_height;
  seq_frames =
    List.map
      (fun gifframe -> {
         Images.frame_left = gifframe.frame_left;
         Images.frame_top = gifframe.frame_top;
         Images.frame_image = Index8 gifframe.frame_bitmap;
         Images.frame_delay = gifframe.frame_delay * 10;
        })
      gifseq.frames;
  seq_loops = gifseq.loops;
}

let load_sequence filename opts = seq_of_gifseq (load filename opts)

let load_first filename opts =
  let sequence = load filename (Load_only_the_first_frame :: opts) in
  let bitmap = (List.hd sequence.frames).frame_bitmap in
  Index8 bitmap

let save filename opts sequence =
  let interlace = Images.save_interlace opts in

  let oc = eGifOpenFileName filename in
  debug_endline "OPENED";

  let color_resolution =
    let max = ref 0 in
    List.iter (fun frame ->
      let colors = Color.size frame.frame_bitmap.colormap in
      if  colors > !max then max := colors ) sequence.frames;
    let colors = !max in
    let rec sub k k' =
      if colors < k' then k
      else sub (k + 1) (k' * 2) in
    let res = sub 0 1 in
    if res = 0 then 1 else res in

  try
    debug_endline "SDESC";
    eGifPutScreenDesc oc {
       s_width = sequence.screen_width;
       s_height = sequence.screen_height;
       s_color_resolution = color_resolution;
       s_back_ground_color = 0;
       s_colormap=
         (List.hd sequence.frames).frame_bitmap.colormap.map
     };

    (* write loops *)
    let loop_written = ref false in
    if sequence.loops <> 0 then begin
      let str = Bytes.create 3 in
      Bytes.unsafe_set str 0 @@ '\001';
      Bytes.unsafe_set str 1 @@ char_of_int (sequence.loops mod 256);
      Bytes.unsafe_set str 2 @@ char_of_int (sequence.loops / 256);
      eGifPutExtension oc
        (gif_make_extension (GifApplication ["NETSCAPE2.0"; Bytes.to_string str]));
      loop_written := true
    end;

    List.iter (fun frame ->
      let graphics_ext : bytes option ref = ref None in
      List.iter
        (fun ext ->
           debug_endline "SEXT";
           try
             begin match ext with
             | GifApplication ["NETSCAPE2.0"; _] ->
                 (* Overridden and written already *)
                 if !loop_written then raise Exit
             | GifGraphics [str] ->
                 (* delayed *)
                 graphics_ext := Some (Bytes.of_string str); raise Exit
             | _ -> ()
             end;
             eGifPutExtension oc (gif_make_extension ext)
           with
           | Exit -> ())
        frame.frame_extensions;

      (* graphics extension *)
      (* If frame_delay and transparent information is different from
         the graphics extension possibly contained in the extensions,
         we override the graphics extension by them *)
      if !graphics_ext <> None || frame.frame_delay <> 0 ||
         frame.frame_bitmap.transparent <> -1 then begin
        let str =
          match !graphics_ext with
          | Some str -> str
          | None -> Bytes.make 4 '\000' in
        if frame.frame_bitmap.transparent <> -1 then begin
          str << 0 & char_of_int ((str @% 0) lor 0x01);
          str << 3 & char_of_int frame.frame_bitmap.transparent
        end else begin
          str << 0 & char_of_int ((str @% 0) land 0xfe);
          str << 3 & '\000'
        end;
        str << 1 & char_of_int (frame.frame_delay mod 256);
        str << 2 & char_of_int (frame.frame_delay / 256);
        eGifPutExtension oc (gif_make_extension (GifGraphics [Bytes.to_string str]))
      end;

      let bmp = frame.frame_bitmap in

      debug_endline "IDESC";
      eGifPutImageDesc oc {
          desc_left = frame.frame_left;
          desc_top = frame.frame_top;
          desc_width = bmp.width;
          desc_height = bmp.height;
          desc_interlace = interlace;
          desc_colormap = bmp.colormap.map;
        };

      let interlace_writer () =
        let rec loop src dest dest_step =
          if dest >= bmp.height then src else begin
            eGifPutLine oc (Index8.get_scanline bmp dest);
            loop (src + 1) (dest + dest_step) dest_step
          end in
      let src_1 = loop     0 0 8 in
      let src_2 = loop src_1 4 8 in
      let src_3 = loop src_2 2 4 in
      ignore (loop src_3 1 2) in

      let normal_writer () =
        for y = 0 to bmp.height - 1 do
          eGifPutLine oc (Index8.get_scanline bmp y)
        done in

      debug_endline "IMAGE";
      if interlace then interlace_writer () else normal_writer ()
    ) sequence.frames;
    eGifCloseFile oc
  with
  | Failure s as e ->
      debug_endline ("ERROR " ^ s);
      eGifCloseFile oc;
      raise e

let save_image name opts image =
  match image with
  | Index8 bmp ->
      save name opts {
         screen_width = bmp.width;
         screen_height = bmp.height;
         screen_colormap = bmp.colormap;
         frames = [ {
             frame_left = 0;
             frame_top = 0;
             frame_bitmap = bmp;
             frame_extensions = []; (* not implemented *)
             frame_delay = 0;
            }; ];
         loops = 0;
        }
  | _ -> raise Wrong_image_type

let check_header filename =
  let len = 10 in
  let ic = open_in_bin filename in
  try
    let str = Bytes.create len in
    really_input ic str 0 len;
    close_in ic;
    match Bytes.sub_string str 0 6 with
    | "GIF87a" | "GIF89a" -> {
         header_width = (str @% 6) + (str @% 7) * 256;
         header_height = (str @% 8) + (str @% 9) * 256;
         header_infos = [];
       }
    | _ -> raise Wrong_file_type
  with
  | _ ->
      close_in ic;
      raise Wrong_file_type

let () = add_methods Gif {
  check_header = check_header;
  load = Some load_first;
  save = Some save_image;
  load_sequence = Some load_sequence;
  save_sequence = None;
}
