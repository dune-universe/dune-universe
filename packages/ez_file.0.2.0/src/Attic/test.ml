
type file = { basename : string; kind : file list option }
let file basename = { basename; kind = None }
let dir basename files = { basename; kind = Some files }

let files =
  [
    dir "a" [
      file "ax.ml";
      file "ay.ml";
      file "ax.mli";
    ];
    dir "b" [
      dir "bc" [
      ];
      dir "bd" [
        file "bdy.ml";
      ];
      file "bx.mli";
      file "bz.ml";
    ];
    file "x.ml"
  ]

let dir0 = "tests-ocplib-file"

module Make(M: sig
    val test : string
    include FileSig.FILE_OPERATIONS
    val of_string : string -> t
    val to_string : t -> string
    end) = struct

  let () =
    let dir0 = M.of_string dir0 in
    let dir = M.add_basename dir0 M.test in
    M.make_dir ~p:true dir;
    let rec make dir files  =
      List.iter (fun { basename; kind } ->
          let file = M.add_basename dir basename in
          match kind with
          | None ->
            M.write_file file basename
          | Some files ->
            M.make_dir file;
            make file files
        ) files
    in
    make dir files;
    let check_dir select ext dir =
      let ext2 = ext ^ "2" in
      let files = M.read_dir ~select dir in
      M.write_lines (M.add_suffix dir ext)
        (Array.map M.to_string files);

      let iterator = M.iterator ~select dir in
      let files = ref [] in
      let rec iter iterator =
        match iterator () with
        | None -> ()
        | Some (_path, file) ->
          files := file :: !files;
          iter iterator
      in
      iter iterator;

      M.write_lines_of_list (M.add_suffix dir ext2)
        (List.map M.to_string (List.rev !files));
    in
    check_dir (M.select ()) ".dir" dir;
    check_dir (M.select ~deep:true ()) ".all" dir;
    check_dir (M.select ~deep:true ~glob:"*.ml" ()) ".caml" dir;
    M.remove_dir ~all:true ~glob:"*.mli" dir;
    M.remove_dir ~all:true dir;
    M.remove_dir ~all:true dir0;
    ()



end

module ForString = Make(struct
    type t = string
    let test = "FileString"
    let of_string s = s
    let to_string s = s
    include FileString
  end)

module ForGen = Make(struct
    let test = "FileGen"
    include FileGen
  end)
