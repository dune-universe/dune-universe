(* To use this script, download the JSON files
   - http://colorbrewer2.org/colorbrewer_schemes.js and
   - http://colorbrewer2.org/cmyk.js
   and remove everything at be beginning before the first "{" and the
   final ";" (if any).  In the first file, also quote all keys by,
   say, evaluating (replace-regexp "\\b\\([0-9]+\\):" "\"\\1\":") in Emacs
   and replace all single quotes by double quotes.
   Place both files in the tools/ directory. *)

open Format

type yes_no_maybe = [`Yes | `No | `Maybe]

let string_of_yes_no_maybe = function
  | `Yes -> "`Yes"
  | `No -> "`No"
  | `Maybe -> "`Maybe"

type map = {
    n: int;
    rgb: (int * int * int) list;
    cmyk: (int * int * int * int) list;
    ty: string;
    blind: yes_no_maybe;
    print: yes_no_maybe;
    copy: yes_no_maybe;
    screen: yes_no_maybe;
  }

let write_rgb fh m =
  fprintf fh "@[<3>[| ";
  (match m.rgb with
   | (r,g,b) :: rgb ->
      fprintf fh "Gg.Color.v_srgbi %3d %3d %3d" r g b;
      List.iter (fun (r,g,b) ->
          fprintf fh ";@\nGg.Color.v_srgbi %3d %3d %3d" r g b
        ) rgb;
   | [] -> ());
  fprintf fh "@] |];@\n"

let write_cmyk fh m =
  fprintf fh "@[<3>[| ";
  (match m.cmyk with
   | (c,m,y,k) :: cmyk ->
      fprintf fh "Gg.V4.v %3d. %3d. %3d. %3d." c m y k;
      List.iter (fun (c,m,y,k) ->
          fprintf fh ";@\nGg.V4.v %3d. %3d. %3d. %3d." c m y k
        ) cmyk;
   | [] -> ());
  fprintf fh "@] |];@\n"

let rgb r g b =
  (int_of_string r, int_of_string g, int_of_string b)

module M = Map.Make(String)

(* Keep a list of maps associated to a name. *)
let add_map maps name m =
  match M.find name maps with
  | l -> M.add name (m :: l) maps
  | exception Not_found -> M.add name [m] maps

let rgb_of_string = function
  | `String s -> Scanf.sscanf s "rgb(%d,%d,%d)" (fun r g b -> (r,g,b))
  | j -> failwith("rgb_of_string: " ^ Yojson.Safe.to_string j)

let parse_colors = function
  | `List colors -> List.map rgb_of_string colors
  | j -> failwith("parse_colors: " ^ Yojson.Safe.to_string j)

(* The colorbrewer_schemes way of encoding a "fuzzy value" *)
let fuzzy_of_int = function
  | `Int 0 -> `No
  | `Int 1 -> `Yes
  | `Int 2 -> `Maybe
  | _ -> failwith "fuzzy_of_int"

let fuzzy_prop = function
  | `List [b] -> let b = fuzzy_of_int b in
                 [b; b; b; b; b; b; b; b; b; b; b; b] (* 12 = max length *)
  | `List l ->
     (* Some property lists are not long enough, assume when not set,
        the last item is always `No. *)
     List.map fuzzy_of_int l @ [`No]
  | _ -> failwith "fuzzy_prop"

(* Add the list of color [palettes] for [name]. [blind], [print],
   [copy], and [screen] are list of properties to be consumed in the
   same order than [palettes] is. *)
let rec add_color_palettes maps name ty palettes ~blind ~print ~copy ~screen =
  match palettes, blind, print, copy, screen with
  | (n, colors) :: palettes, b :: blind, p :: print, c :: copy, s :: screen ->
     let n = int_of_string n in
     let rgb = parse_colors colors in
     (* For sequential schemes, revert to go from dark to light. *)
     let rgb = if ty = "seq" then List.rev rgb else rgb in
     let m = {n; rgb; cmyk = [];
              ty; blind = b; print = p; copy = c; screen = s } in
     let maps = add_map maps name m in
     add_color_palettes maps name ty palettes ~blind ~print ~copy ~screen
  | [], _, _, _, _ -> maps
  | _ -> failwith "add_color_palettes"

let process_named_palette maps (name, palettes) =
  match palettes with
  | `Assoc palettes ->
     let props = match List.assoc "properties" palettes with
       | `Assoc p -> p
       | _ -> failwith "process_names: properties" in
     let palettes = List.filter (fun (n,_) -> n <> "properties") palettes in
     let ty = match List.assoc "type" props with
       | `String ty -> ty
       | _ -> failwith "process_named_palette: type" in
     let blind = fuzzy_prop(List.assoc "blind" props) in
     let print = fuzzy_prop(List.assoc "print" props) in
     let copy = fuzzy_prop(List.assoc "copy" props) in
     let screen = fuzzy_prop(List.assoc "screen" props) in
     add_color_palettes maps name ty palettes ~blind ~print ~copy ~screen
  | j -> failwith("process_named_palette: " ^ Yojson.Safe.to_string j)

let process_names maps (json: Yojson.Safe.t) =
  match json with
  | `Assoc l -> List.fold_left process_named_palette maps l
  | _ -> failwith "process_name"


(** [cmyk_of_json json] return the 4-uple for the JSON tutple listing
    the colors. *)
let cmyk_of_json = function
  | `List [`Int c; `Int m; `Int y; `Int k] -> (c,m,y,k)
  | j -> failwith("cmyk_of_json: " ^ Yojson.Safe.to_string j)

(** Get the CMYK information from [json] for the palette [name] with
    [n] colors. *)
let get_cmyk (json: Yojson.Safe.t) name n =
  match json with
  | `Assoc l ->
     (match List.assoc name l with
      | `Assoc maps ->
         (match List.assoc (string_of_int n) maps with
          | `List colors ->
             List.map cmyk_of_json colors
          | j -> failwith("JSON: " ^ Yojson.Safe.to_string j))
      | _ -> failwith(sprintf "JSON entry %s is not an associated list" name))
  | _ -> failwith "JSON is not an associative list"

let add_cmyk json maps =
  M.mapi (fun name ms ->
      List.map (fun m -> { m with cmyk = get_cmyk json name m.n }) ms
    ) maps

let array_string proj l =
  let l = List.map proj l |> List.map string_of_yes_no_maybe in
  "[| " ^ String.concat "; " l ^ " |]"

let write_cmyk_of_rgb ft c =
  let r, g, b = Gg.Color.(r c, g c, b c) in (* ∈ [0,1] *)
  let k' = max r (max g b) in
  let k = 1. -. k' in
  let c = (1. -. r -. k) /. k' in
  let m = (1. -. g -. k) /. k' in
  let y = (1. -. b -. k) /. k' in
  fprintf ft "Gg.V4.v %f %f %f %f;@\n" c m y k


let () =
  let dir = Filename.dirname Sys.argv.(0) in
  let rgb_json_fname = Filename.concat dir "colorbrewer_schemes.js" in
  let cmyk_json_fname = Filename.concat dir "cmyk.js" in
  let rgb_json = Yojson.Safe.from_file rgb_json_fname in
  let cmyk_json = Yojson.Safe.from_file cmyk_json_fname in
  let brewer_maps = process_names M.empty rgb_json |> add_cmyk cmyk_json in
  let fh = open_out "src/palettes.ml" in
  let ft = Format.formatter_of_out_channel fh in
  fprintf ft "(* Written by %s *)\n\
              open Palette_t\n@\n" Sys.argv.(0);
  fprintf ft "(* Brewer colormaps — see http://colorbrewer2.org/\n   \
              Number of maps: %d *)@\n"
    (M.fold (fun _ _ n -> n+1) brewer_maps 0);
  M.iter (fun name ms ->
      let ms = List.sort (fun m1 m2 -> compare m1.n m2.n) ms in
      let name = String.lowercase_ascii name in
      let n_min = List.fold_left (fun n m -> min n m.n) max_int ms in
      let n_max = List.fold_left (fun n m -> max n m.n) 0 ms in
      let ms =
        if n_min = 3 then (
          (* Complete the ranges of length 1, 2 *)
          let m3 = List.hd ms in
          { m3 with n = 1;  rgb = [List.hd m3.rgb];
                       cmyk = [List.hd m3.cmyk] }
          :: { m3 with n = 2;  rgb = [List.hd m3.rgb; List.nth m3.rgb 2];
                       cmyk = [List.hd m3.cmyk; List.nth m3.cmyk 2] }
          :: ms
        )
        else assert false in
      List.iter (fun m ->
          fprintf ft "@[<4>let %s_%i = {@\n\
                      length = %i;@\n\
                      rgb = " name m.n m.n;
          write_rgb ft m;
          fprintf ft "cmyk = ";
          write_cmyk ft m;
          fprintf ft "ty = `%s;@\n\
                      blind = %s;@\n\
                      print = %s;@\n\
                      copy = %s;@\n\
                      screen = %s }@]@\n"
            (String.capitalize_ascii m.ty)
            (string_of_yes_no_maybe m.blind)
            (string_of_yes_no_maybe m.print)
            (string_of_yes_no_maybe m.copy)
            (string_of_yes_no_maybe m.screen);
        ) ms;
      (* Group maps with the same "scheme". *)
      fprintf ft "@[<4>let %s : t list = [@\n" name;
      for i = 1 to n_max do
        fprintf ft "%s_%i;@ " name i
      done;
      fprintf ft "]@]@\n@\n";
    ) brewer_maps;
  fprintf ft "(* Matplotlib colormaps *)@\n";
  let matplotlib_maps =
    Matplotlib.([ "magma", magma_rgb; "inferno", inferno_rgb;
                  "plasma", plasma_rgb; "viridis", viridis_rgb]) in
  List.iter (fun (name, rgb) ->
      fprintf ft "@[<4>let %s = {@\n\
                  length = %i;@\n\
                  rgb = @[<3>[| " name (Array.length rgb);
      Array.iter (fun c -> let r, g, b = Gg.Color.(r c, g c, b c) in
                           fprintf ft "Gg.Color.v %f %f %f 1.;@\n" r g b) rgb;
      fprintf ft "|];@]@\ncmyk = @[<3>[| ";
      Array.iter (write_cmyk_of_rgb ft) rgb;
      fprintf ft "|];@]@\n\
                  ty = `Seq;@\n\
                  blind = `Yes;@\n\
                  print = `Yes;@\n\
                  copy = `Yes;@\n\
                  screen = `Yes }@]@\n@\n"
    ) matplotlib_maps;
  (* Write a list of all maps (e.g. for search). *)
  fprintf ft "@[<4>let all_maps = [@\n";
  M.iter (fun name ms ->
      let name = String.lowercase_ascii name in
      let n_max = List.fold_left (fun n m -> max n m.n) 0 ms in
      for i = 1 to n_max do
        fprintf ft "%s_%i;@ " name i
      done
    ) brewer_maps;
  List.iter (fun (name, _) ->
      fprintf ft "%s;@ " name
    ) matplotlib_maps;
  fprintf ft "]@]@\n";
  close_out fh



(* Local Variables: *)
(* compile-command: "make -k -w -C.. preconfigure" *)
(* End: *)
