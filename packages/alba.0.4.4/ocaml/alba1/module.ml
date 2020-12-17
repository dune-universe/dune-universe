(* Copyright (C) 2017 Helmut Brandl  <helmut dot brandl at gmx dot net>

   This file is distributed under the terms of the GNU General Public License
   version 2 (GPLv2) as published by the Free Software Foundation.
*)

open Support
open Container

module Option = Fmlib.Option

let max_modification_time (t1:float option) (t2:float option): float option =
  match t1, t2 with
  | Some t1, Some t2 ->
     if t1 <= t2 then
       Some t2
     else
       Some t1
  | _ ->
     None



type 'a parse_function = (Lexing.lexbuf  -> Parser.token) -> Lexing.lexbuf -> 'a


let abort (str:string) =
  prerr_string (str ^ "\n"); exit 1


let info_abort (fn:string) (info:Support.info) (str:string) =
  abort ((Support.info_string info) ^ " " ^ str)


let parse (fn:string) (parse_function: 'a parse_function): 'a =
  (* Parse the file [fn] by using the parse function [parse_function]. *)
  try
    let ch_in = open_in fn in
    let lexbuf = Lexing.from_channel ch_in in
    Lexer.initialize fn lexbuf;
    let res = parse_function Lexer.token lexbuf in
    close_in ch_in;
    res
  with
    Parsing.Parse_error ->
      info_abort fn (Lexer.info_of_last_pos ()) "Unexpected token"
  | Support.Error_info (info,str) ->
      info_abort fn info str
  | Sys_error str ->
      abort str



let use_block (fn:string): Support.use_block =
  parse fn Parser.use_block_opt



module Src =
  struct
    type t = {
        dir:  string;
        name: string;
        pkg: library_name;
        dependencies: module_name withinfo list;
        mtime:  float;         (* mod time of source file *)
        mtime2: float option;  (* mod time of json file, if exists *)
        mutable mtime3: float option;  (* max mod time of all dependencies,
                                          if exist *)
        mutable full_dependencies: int list;
      }

    let directory (src:t): string =
      src.dir

    let name (src:t): string =
      src.name

    let path (src:t): string =
      Platform.Filename.concat src.dir src.name

    let package (s:t): library_name =
      s.pkg

    let json_path (s:t): string =
      let module FN = Platform.Filename in
      FN.concat (FN.concat (directory s) ".alba") ((name s) ^ ".json")


    let is_new (src:t): bool =
      match src.mtime2 with
      | None ->
         true
      | _ ->
         false


    let is_modified (src:t): bool =
      match src.mtime2 with
      | None ->
         true
      | Some t2 ->
         t2 < src.mtime


    let is_affected (src:t): bool =
      let open Format in
      match src.mtime2, src.mtime3, src.dependencies with
      | Some t2, None, [] ->
         t2 < src.mtime
      | Some t2, Some t3, _ ->
         assert (src.dependencies <> []);
         t2 < src.mtime || t2 < t3
      | Some _, None, _::_  | None, _, _ ->
         true


    let max_modification_time (src:t): float option =
      max_modification_time
        (Some src.mtime)
        (if src.dependencies = [] then
           src.mtime2
         else
           max_modification_time src.mtime2 src.mtime3)


    let dependencies (src:t): module_name withinfo list =
      src.dependencies


    let full_dependencies (src:t): int list =
      src.full_dependencies

    let set_full_dependencies (lst:int list) (mt:float option) (src:t): unit =
      src.full_dependencies <- lst;
      src.mtime3 <- mt

    let qualified_module_name
          (name:module_name withinfo) (pkg:library_name)
        : module_name withinfo =
      let n,p = name.v in
      if p = [] then
        withinfo name.i (n,pkg)
      else
        name


    let get (dir:string) (name:string) (pkg:library_name): t =
      let module FN = Platform.Filename in
      let lname = FN.concat dir name in
      let mtime = Platform.modification_time lname in
      let mtime2 =
        let json_name = FN.concat (FN.concat dir ".alba") (name ^ ".json") in
        try
          Some (Platform.modification_time json_name)
        with Sys_error _ ->
          None
      and deps =
        List.map (fun n -> qualified_module_name n pkg) (use_block lname)
      in
      {dir  = dir;
       name = name;
       pkg = pkg;
       dependencies = deps;
       mtime  = mtime;
       mtime2 = mtime2;
       mtime3 = None;
       full_dependencies = []
      }

    let parse (s:t): declaration list =
      (* Parse the declarations of the source [s] and throw away the use block. *)
      let use_blk, ast = parse (path s) Parser.file in
      ast

    let info_abort (info:info) (str:string) (s:t): 'a =
      info_abort (path s) info str

    let write_meta (s:t): unit =
      if is_affected s then
        Platform.write_dummy (json_path s)

  end (* Src *)




module M =
  struct
    type sourcefiles =
      | Both of Src.t * Src.t
      | Implementation of Src.t
      | Interface of Src.t

    type t = {
        name: module_name;
        sources: sourcefiles;
        mutable id: int option
      }


    let compare (m1:t) (m2:t): int =
      Stdlib.compare m1.name m2.name

    let base_name (m:t): int =
      fst m.name

    let package_name (m:t):library_name =
      snd m.name

    let primary_source (m:t): Src.t =
      match m.sources with
      | Both (imp, ifce) ->
         imp
      | Implementation imp ->
         imp
      | Interface ifce ->
         ifce


    let name (m:t): module_name =
      m.name


    let string_of_name (m:t): string =
      string_of_module m.name


    let is_external (m:t): bool =
      let _,pkg = m.name in
      pkg <> []


    let is_affected (m:t): bool =
      let is_aff src = Src.is_affected src in
      match m.sources with
      | Both (impl, ifc) ->
         is_aff impl || is_aff ifc
      | Implementation impl ->
         is_aff impl
      | Interface ifc ->
         is_aff ifc

    let has_interface (m:t): bool =
      match m.sources with
      | Both _ | Interface _ ->
         true
      | _ ->
         false

    let has_implementation (m:t): bool =
      match m.sources with
      | Both _ | Implementation _ ->
         true
      | _ ->
         false

    let interface (m:t): Src.t =
      match m.sources with
      | Both (_,ifc) | Interface ifc ->
         ifc
      | _ ->
         assert false

    let implementation (m:t): Src.t =
      match m.sources with
      | Both (imp,_) | Implementation imp ->
         imp
      | _ ->
         assert false

    let has_id (m:t): bool =
      match m.id with
        None -> false
      | Some _ -> true

    let id (m:t): int =
      assert (has_id m);
      Option.value m.id

    let put_sorted (id:int) (m:t): unit =
      assert (not (has_id m));
      m.id <- Some id

    let equal (m1:t) (m2:t): bool =
      match m1.id, m2.id with
      | Some id1, Some id2 ->
         id1 = id2
      | _ , _ ->
         assert false (* call not allowed unless sorted *)

    let same_package (m1:t) (m2:t): bool =
      let _,p1 = m1.name
      and _,p2 = m2.name in
      p1 = p2

    let uses0 (public:bool) (m1:t) (m2:t): bool =
      (* Does the module [m1] use (publicly?) the module [m2]? *)
      let uses_public id2 =
        has_interface m1
        && List.mem id2 (Src.full_dependencies (interface m1))
      and uses_private id2 =
        has_implementation m1
        && List.mem id2 (Src.full_dependencies (implementation m1))
      in
      match m1.id, m2.id with
      | Some id1, Some id2 ->
         id1 = id2
         || if public then
              uses_public id2
            else
              uses_public id2 || uses_private id2
      | _, _ ->
         assert false (* call not allowed unless sorted *)

    let uses_public (m1:t) (m2:t): bool =
      uses0 true m1 m2

    let uses (m1:t) (m2:t): bool =
      uses0 false m1 m2

    let get (path:string) (name:string) (mname:module_name): t =
      let src_al =
        try
          Src.get path (name ^ ".al") []
        with Sys_error _ ->
          Format.eprintf
            "The module \"%s\" does not have an implementation file@."
            (string_of_module mname);
          raise Not_found
      in
      try
        let src_ali = Src.get path (name ^ ".ali") [] in
        { name = mname; sources = Both (src_al, src_ali); id = None }
      with Sys_error _ ->
        { name = mname; sources = Implementation src_al; id = None }

    let get_external (path:string) (name:string) (mnme:module_name withinfo): t =
      let src_ali =
        try
          Src.get path (name ^ ".ali") (snd mnme.v)
        with Sys_error _ ->
          let open Format in
          eprintf
            "%s The module \"%s\" does not exist@."
            (info_string mnme.i)
            (string_of_module mnme.v);
          exit 1
      in
      {name = mnme.v; sources = Interface src_ali; id = None}

  end (* M *)






module PSet =
  struct
    type t = {
        cmd: Command_line.t;
        paths: string list;
        mutable map: string Library_map.t;
      }

    let make (cmd:Command_line.t): t =
      let wdir = Command_line.working_directory cmd in
      let paths =
        let paths1 = Command_line.package_paths cmd in
        try
          let paths2 =
            try
              Mystring.split
                (Platform.getenv "ALBA_LIBRARY_PATH")
                (Platform.path_separator ())
            with Not_found ->
              []
          and paths3 =
            try
              Platform.system_with_output "opam config var --yes alba:lib"
            with Sys_error str ->
              []
          in
          paths1 @ paths2 @ paths3
        with Not_found ->
          paths1
      in
      {cmd = cmd;
       paths = paths;
       map = Library_map.singleton [] wdir
      }

    let find (p:library_name) (info:info) (s:Src.t) (ps:t): string =
      try
        Library_map.find p ps.map
      with Not_found ->
        let module FN = Platform.Filename in
        let open Format in
        let pstr = string_of_library p in
        let path =
          try
            List.find
              (fun dir -> Platform.is_directory (FN.concat dir pstr))
              ps.paths
          with Not_found ->
            eprintf
              "%s Cannot find the package \"%s\"@."
              (info_string info)
              (string_of_library p);
            assert false
                   (*exit 1*)
        in
        let path = FN.concat path pstr in
        let alba_path = FN.concat path ".alba" in
        if not (Platform.path_exists alba_path &&
                  Platform.is_directory alba_path) then
          begin
            eprintf
              "@[<v>%s@,@,  %s@,@,%s@,@,  %s@,@,%s@]@."
              "I have found the package"
              (string_of_library p)
              "in the directory"
              path
              "but it is not a valid Alba directory (not initialized).";
            exit 1
          end;
        ps.map <- Library_map.add p path ps.map;
        path

  end (* PSet *)





let max_in_int_lists (lstlst:int list list): int =
  List.fold_left
    (fun max lst ->
      match lst with
      | [] ->
         assert false (* no empty lists allowed *)
      | hd :: _ ->
         if max < hd then
           hd
         else
           max
    )
    0
    lstlst


let remove_max_in_int_lists (max:int) (lstlst:int list list): int list list =
  List.fold_left
    (fun lstlst lst ->
      match lst with
      | hd::tl ->
         if hd = max then
           if tl = [] then
             lstlst
           else
             tl :: lstlst
         else
           lst :: lstlst
      | _ ->
         assert false (* no empty lists allowed *)
    )
    []
    lstlst


let merge_int_lists (lstlst: int list list): int list =
  let rec merge (lstlst: int list list) (res:int list): int list =
    if lstlst = [] then
      res
    else
      let max = max_in_int_lists lstlst
      in
      let lstlst = remove_max_in_int_lists max lstlst
      in
      merge lstlst (max::res)
  in
  merge lstlst []





module MSet =
  struct
    type node = M.t

    type t = {
        cmd: Command_line.t;
        mutable map: M.t Module_map.t;
        seq: M.t Seq.t;
        ps: PSet.t
      }

    type graph = t

    let make (cmd:Command_line.t) (map: M.t Module_map.t): graph =
      {cmd = cmd; map = map; seq = Seq.empty ();
       ps = PSet.make cmd}

    let map (g:graph): M.t Module_map.t = g.map

    let find (nme:module_name) (set:graph): M.t =
      Module_map.find nme set.map

    let must_find (nme:module_name) (set:graph): M.t =
      try
        find nme set
      with Not_found ->
        assert false (* Module has to be in the map *)


    let verbosity (set:t): int =
      Command_line.verbosity set.cmd

    let string_of_node (m:node): string =
      string_of_module (M.name m)

    let compare (m1:node) (m2:node): int =
      M.compare m1 m2

    let count_sorted (set:t): int =
      Seq.count set.seq

    let has_id (i:int) (set:t): bool =
      i < count_sorted set

    let module_of_id (i:int) (set:graph): M.t =
      assert (i < Seq.count set.seq);
      Seq.elem i set.seq


    let mtime_dependencies (deps:int list) (set:graph): float option =
      let rec mtime
                (first:bool)
                (deps:int list)
                (res:float option)
              : float option =
        match deps with
        | [] ->
           res
        | i :: tl ->
           let m = module_of_id i set in
           let ifc = M.interface m in
           let mt = Src.max_modification_time ifc in
           if first then
             mtime false tl mt
           else
             mtime false tl (max_modification_time res mt)
      in
      mtime true deps None


    let set_full_dependencies
          (is_interface:bool) (src:Src.t) (set:graph)
        : unit =
      let full_deps =
        let deps = Src.dependencies src
        in
        let lstlst =
          List.rev_map
            (fun nme ->
              let m_used =
                try
                  find nme.v set
                with Not_found ->
                  begin
                    if not is_interface then
                      assert false; (* cannot happen in implementation file *)
                    Format.eprintf
                      "@[%s %s \"%s\" %s@]@."
                      (info_string nme.i)
                      "The module"
                      (string_of_module nme.v)
                      "has not been used in any implementation file";
                    exit 1
                  end
              in
              if not (M.has_interface m_used) then
                begin
                  Format.eprintf
                    "%s The module \"%s\" does not have an interface@."
                    (info_string nme.i)
                    (M.string_of_name m_used);
                  exit 1
                end;
              M.id m_used
              :: (M.interface m_used |> Src.full_dependencies |> List.rev)
            )
            deps
        in
        merge_int_lists lstlst
      in
      let mt_deps = mtime_dependencies full_deps set in
      Src.set_full_dependencies full_deps mt_deps src



    let put_sorted (m:node) (set:graph): unit =
      assert (not (M.has_id m));
      let i = Seq.count set.seq in
      M.put_sorted i m;
      Seq.push m set.seq;
      if M.has_interface m then
        set_full_dependencies true (M.interface m) set;
      if M.has_implementation m then
        set_full_dependencies false (M.implementation m) set


    let dependencies (m:node) (set: t): node list =
      let deps =
        if M.has_implementation m then
          Src.dependencies (M.implementation m)
        else
          []
      in
      let deps =
        if M.has_interface m then
          Src.dependencies (M.interface m) @ deps
        else
          deps
      in
      let src = M.primary_source m in
      List.fold_left
        (fun lst mnme ->
          let nme,pkg = mnme.v in
          try
            Module_map.find mnme.v set.map :: lst
          with Not_found ->
            let dir = PSet.find pkg mnme.i src set.ps in
            let m =
              if M.is_external m || pkg <> [] then
                M.get_external dir (ST.string nme) mnme
              else
                try
                  M.get dir (ST.string nme) mnme.v
                with Not_found ->
                  Format.eprintf
                    "@[%s %s \"%s\" %s@]@."
                    (info_string mnme.i)
                    "The module"
                    (ST.string nme)
                    "does not exist.";
                  exit 1
            in
            set.map <- Module_map.add mnme.v m set.map;
            m :: lst
        )
        []
        deps

    let fold (f:'a -> M.t -> 'a) (start:'a) (set:t): 'a =
      Seq.fold f start set.seq

    let iter (f:M.t->unit) (set:t): unit =
      Seq.iter f set.seq


    let verify_dependencies (m:M.t) (set:t) : unit =
      assert (M.has_implementation m);
      assert (M.has_interface m);
      let impl = M.implementation m
      and ifc  = M.interface m in
      List.iter
        (fun nme ->
          let m_used = must_find nme.v set in
          if not (List.mem (M.id m_used) (Src.full_dependencies impl))
          then
            Src.info_abort
              nme.i
              ("module \"" ^ M.string_of_name m_used
               ^ "\" not used in implementation file")
              ifc
        )
        (Src.dependencies ifc)
  end (* MSet *)



module Topisort = Topological_sort.Make (MSet)



let is_alpha_num (c:char): bool =
  c = '_' ||
  let code = Char.code c
  and a    = Char.code 'a'
  and z    = Char.code 'z'
  and zero = Char.code '0'
  and nine = Char.code '9' in
  (a    <= code && code <= z) ||
  (zero <= code && code <= nine)


let is_alba_identifier (str:string): bool =
  Mystring.for_all is_alpha_num str


let  module_of_source_name (src:string): string =
  (* Extract from the source file name the module name. Raise [Not_found] if
     [src] is not a valid source file name. *)
  let module FN = Filename in
  if not (FN.check_suffix src ".al" || FN.check_suffix src ".ali") then
    raise Not_found;
  let str = FN.chop_extension src in
  if is_alba_identifier str then
    str
  else
    raise Not_found


let is_alba_source_file_name (str:string): bool =
  (* Is [str] a valid Alba source file name i.e. is its extension either .al
     or .ali and is the module name a valid Alba identifier. *)
  try
    ignore(module_of_source_name str);
    true
  with Not_found ->
    false







let read_module_names (dir:string): StringSet.t =
  (* Scan the directory [dir] for Alba source file names and return the set
     of all modules. A [Sys_error] is raised if [dir] is not a directory
     which can be read. *)
  let dir =
    if dir = "" then
      Platform.getcwd ()
    else
      dir
  in
  Array.fold_left
    (fun set name ->
      try
        StringSet.add (module_of_source_name name) set
      with Not_found ->
        set
    )
    StringSet.empty
    (Platform.readdir dir)





let read_module_infos
      (wdir:string)
      (args:string list)
    : M.t Module_map.t * M.t list =
  (* Read the modules in [args] in the directory [wdir] or all modules in [wdir]
     in case that [args] is empty. Put the module infos into a map which maps
     module names to module info and a list of module infos.   *)
  let mlist =
    match args with
    | [] ->
       wdir |> read_module_names |> StringSet.elements
    | _ ->
       args
  in
  List.fold_left
    (fun (map,lst) mname ->
      let nme = ST.symbol mname, [] in
      if Module_map.mem nme map then
        map, lst
      else
        let m =
          try
            M.get wdir mname nme
          with Not_found ->
            Format.eprintf "Cannot find the module %s@." mname;
            exit 1
        in

        Module_map.add nme m map, m :: lst
    )
    (Module_map.empty,[])
    mlist






let make_set (cmd:Command_line.t): MSet.t =
  let wdir = Command_line.working_directory cmd
  and args = Command_line.arguments cmd
  in
  let map,lst = read_module_infos wdir args in
  let graph = MSet.make cmd map in
  let lst =
    match Topisort.sort lst graph with
    | Ok lst ->
       lst
    | Error cycle ->
       let f = Format.err_formatter in
       Format.fprintf
         f
         "@[<v>@,%s@,@[<v 4>@,"
         "I have discovered a cyclic module dependency";
       Topisort.print_cycle f cycle;
       Format.fprintf f "@]@]@.";
       exit 1
  in
  List.iter
    (fun m -> MSet.put_sorted m graph)
    lst;
  graph



module Compile =
  struct
    type t = {
        set: MSet.t;
        target:  M.t;  (* The module to be compiled *)
        current: M.t;  (* The currently parsed module *)
        mode: int;     (* 0: interface private use,
                          1: interface public use,
                          2: implementation verification,
                          3: interface verification *)
      }
    let verbosity (c:t): int =
      MSet.verbosity c.set
    let set (c:t): MSet.t = c.set
    let current (c:t): M.t = c.current
    let target  (c:t): M.t = c.target
    let current_is_target (c:t): bool =
      M.equal c.current c.target
    let is_interface_use (c:t): bool =
      c.mode <= 1
    let is_interface_public_use (c:t): bool =
      c.mode = 1
    let is_verifying (c:t): bool =
      c.mode = 2
    let is_interface_check (c:t): bool =
      c.mode = 3
    let is_publicly_visible (m:M.t) (c:t): bool =
      M.has_interface c.target
      && List.mem (M.id m) (c.target |> M.interface |> Src.full_dependencies)

    let make (m:M.t) (set:MSet.t): t =
      assert (M.has_id m);
      { set = set; target = m; current = m; mode = 2}
    let set_current (m:M.t) (c:t): t =
      assert (M.has_interface m || M.equal m c.target);
      let mode =
        if M.equal m c.target then
          2
        else if M.has_interface c.target
                && List.mem
                     (M.id m)
                     (c.target |> M.interface |> Src.full_dependencies)
        then
          1
        else
          0
      in
      {c with
        mode = mode;
        current = m}
    let set_interface_check (c:t): t =
      assert (current_is_target c);
      {c with mode = 3}
  end
