[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type t = {
  map : Owee_buf.t;
  sections : Owee_elf.section array;
  strtab : Owee_elf.String_table.t option;
  symtab : Owee_elf.Symbol_table.t option;
  resolved : (Int64.t, (string * int) option) Hashtbl.t;
}

let create ~elf_executable =
  let fd = Unix.openfile elf_executable [Unix.O_RDONLY] 0 in
  let len = Unix.lseek fd 0 Unix.SEEK_END in
  let map =
    Versioned.map_file fd
      Bigarray.int8_unsigned Bigarray.c_layout false len
  in
  Unix.close fd;
  let _header, sections = Owee_elf.read_elf map in
  let resolved = Hashtbl.create 42 in
  let strtab = Owee_elf.find_string_table map sections in
  let symtab = Owee_elf.find_symbol_table map sections in
  { map; sections; strtab; symtab; resolved; }

(* CR mshinwell: tidy all this up.  Also, the pinpointing of which row
   is the correct one isn't great. *)

exception Found of string * int

let resolve_from_dwarf t ~program_counter =
  (* CR-soon mshinwell: owee should use Int64.t *)
  let program_counter' = program_counter in
  let program_counter = Int64.to_int program_counter in
  try
    begin match Owee_elf.find_section t.sections ".debug_line" with
    | None -> ()
    | Some section ->
      let body = Owee_buf.cursor (Owee_elf.section_body t.map section) in
      let rec aux () =
        match Owee_debug_line.read_chunk body with
        | None -> ()
        | Some (header, chunk) ->
          (* CR-soon mshinwell: fix owee .mli to note that [state] is
             mutable!  It should have a copy function or something too. *)
          let copy_state (state : Owee_debug_line.state)
                : Owee_debug_line.state =
            { address = state.address;
              filename = state.filename;
              file = state.file;
              line = state.line;
              col = state.col;
              is_statement = state.is_statement;
              basic_block = state.basic_block;
              end_sequence = state.end_sequence;
              prologue_end = state.prologue_end;
              epilogue_begin = state.epilogue_begin;
              isa = state.isa;
              discriminator = state.discriminator;
            }
          in
          let check header (state : Owee_debug_line.state)
                (prev_state : Owee_debug_line.state option)
                : Owee_debug_line.state option =
            (* N.B. [state] is mutable! *)
            match state.end_sequence, prev_state with
            | _, None
            | true, _ ->
              let state = copy_state state in
              Some state
            | _, Some prev_state ->
              if program_counter >= prev_state.address
                && program_counter < state.address then
              begin
                match Owee_debug_line.get_filename header state with
                | None -> raise (Found (prev_state.filename, prev_state.line))
                | Some filename -> raise (Found (filename, prev_state.line))
              end else begin
                let state = copy_state state in
                Some state
              end
          in
          ignore (Owee_debug_line.fold_rows (header, chunk) check None);
          aux ()
      in
      aux ()
    end;
    Hashtbl.add t.resolved program_counter' None;
    None
  with (Found (filename, line)) -> begin
    let result = Some (filename, line) in
    Hashtbl.add t.resolved program_counter' result;
    result
  end

let resolve t ~program_counter =
  match Hashtbl.find t.resolved program_counter with
  | resolved -> resolved
  | exception Not_found -> resolve_from_dwarf t ~program_counter

let function_at_pc t ~program_counter:address =
  match t.symtab, t.strtab with
  | None, None | Some _, None | None, Some _ -> None
  | Some symtab, Some strtab ->
    match
      Owee_elf.Symbol_table.functions_enclosing_address symtab ~address
    with
    | [] -> None
    (* Just take the first one for the moment.  There will usually be
       only one. *)
    | sym::_ -> Owee_elf.Symbol_table.Symbol.name sym strtab
