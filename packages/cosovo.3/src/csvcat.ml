(* apply [f] to each element of the input list, calling [sep]
   inbetween such elements *)
let rec iter_sep f sep = function
  | a :: b :: rest ->
      f a;
      sep ();
      iter_sep f sep (b :: rest)

  | [ a ] ->
      f a

  | [] ->
      ()

let string_of_value = function
  | `Int i -> string_of_int i
  | `Float f -> string_of_float f
  | `String s -> "\"" ^ s ^ "\""

let pr_strings ?(sep=",") out strings =
  out (String.concat sep strings);
  out "\n"

let pr_dense_row out row =
  iter_sep (fun v -> print_string (string_of_value v))
    (fun () -> out ",") row;
  out "\n"

let pr_sparse_row out pairs =
  out "{";
  iter_sep (
    fun (feature_id, v) ->
      out (string_of_int feature_id);
      out " ";
      out (string_of_value v)
  ) (fun () -> out ",") pairs;
  out "}\n"


let pr_dense_subset_row out row column_included =
  let _ = List.fold_left (
    fun (has_prev, index) value ->
      if column_included index then (
        if has_prev then
          out ",";

        out (string_of_value value);
        true, index + 1
      )
      else
        has_prev, index + 1
  ) (false, 0) row in
  out "\n"


let pr_sparse_subset_row out row column_included =
  out "{";
  let _ = List.fold_left (
    fun has_prev (index, value) ->
      if column_included index then (
        if has_prev then
          out ",";

        out (string_of_int index);
        out " ";
        out (string_of_value value);
        true
      )
      else
        has_prev

  ) false row in
  out "}\n"


type incl_excl_spec = {
  start_with : [ `All | `None ] ;
  commands : ([`Include | `Exclude ] * Re.Pcre.regexp) list
}

type column = int * string (* a column's name with its 0-based index *)

module ColumnSet = Set.Make(
  struct
    type t = column
    let compare = Stdlib.compare
  end
  )

let rec apply_commands incl_excl_sets = function
  | (`Include, rex) :: rest ->
    (* move elements from [excluded_set] to [included_set] *)
    let _, excluded_set = incl_excl_sets in

    let incl_excl_sets = ColumnSet.fold (
      fun element incl_excl_sets ->
        let included_set, excluded_set = incl_excl_sets in
        let _, column_name = element in
        if Re.Pcre.pmatch ~rex column_name then
          ColumnSet.add element included_set,
          ColumnSet.remove element excluded_set
        else
          incl_excl_sets
    ) excluded_set incl_excl_sets in
    apply_commands incl_excl_sets rest

  | (`Exclude, rex) :: rest ->
    (* move elements from [included_set] to [excluded_set] *)
    let included_set, _ = incl_excl_sets in

    let incl_excl_sets = ColumnSet.fold (
      fun element incl_excl_sets ->
        let included_set, excluded_set = incl_excl_sets in
        let _, column_name = element in
        if Re.Pcre.pmatch ~rex column_name then
          ColumnSet.remove element included_set,
          ColumnSet.add element excluded_set
        else
          incl_excl_sets
    ) included_set incl_excl_sets in
    apply_commands incl_excl_sets rest

  | [] -> incl_excl_sets


let rec parse_commands accu = function
  | "i" :: regexp :: rest ->
    let accu = (`Include, Re.Pcre.regexp regexp) :: accu in
    parse_commands accu rest

  | "x" :: regexp :: rest ->
    let accu = (`Exclude, Re.Pcre.regexp regexp) :: accu in
    parse_commands accu rest

  | [] ->
    List.rev accu

  | other :: _ ->
    Printf.printf "unknown command %S; must be either \"i\" (for include) \
      or \"x\" (for exclude)\n%!" other;
    exit 1


let parse_incl_excl_spec = function
  | "all" :: rest ->
    let commands = parse_commands [] rest in
    { start_with = `All; commands }

  | "none" :: rest ->
    let commands = parse_commands [] rest in
    { start_with = `None; commands }

  | (("i" | "x") :: _ as commands)  ->
    (* assume [`All] when ["i"] and ["x"] appear without a starting
       set specifier *)
    let commands = parse_commands [] commands in
    { start_with = `All; commands }

  | _ ->
    print_endline "column subset specifier must begin with \"all\" \
      (to start with all columns) or \"none\" to start with no columns.";
    exit 1

let column_set_of_header = function
  | `Sparse header ->
    let num_columns, set = List.fold_left (
      fun (num_columns, set) (i, name) ->
        let num_columns = max i num_columns in
        num_columns, ColumnSet.add (i, name) set
    ) (0, ColumnSet.empty) header in
    num_columns + 1, set

  | `Dense header ->
    List.fold_left (
      fun (i, set) name ->
        i + 1, ColumnSet.add (i, name) set
    ) (0, ColumnSet.empty) header

let included_columns_of_spec header incl_excl_spec_as_list =
  let incl_excl_spec = parse_incl_excl_spec incl_excl_spec_as_list in
  let num_columns, column_set = column_set_of_header header in
  let incl_excl_set =
    match incl_excl_spec.start_with with
    | `All -> column_set, ColumnSet.empty
    | `None -> ColumnSet.empty, column_set
  in
  let included_set, _excluded_set =
    apply_commands incl_excl_set incl_excl_spec.commands in
  let is_included = Array.make num_columns false in
  ColumnSet.iter (
    fun (index, _) ->
      is_included.(index) <- true
  ) included_set;
  Array.get is_included

let pr_subset_of_columns : (int -> bool) -> (string -> unit) -> Cosovo.IO.row_or_error -> unit =
  fun is_included out -> function
    | Ok (`Dense dense) ->
      pr_dense_subset_row out dense is_included

    | Ok (`Sparse sparse) ->
      pr_sparse_subset_row out sparse is_included

    | Error err ->
      print_endline (Cosovo.IO.string_of_error err);
      exit 1


let pr_columns out = function
  | Error err ->
    print_endline (Cosovo.IO.string_of_error err);
    exit 1

  | Ok (`Dense dense) ->
    pr_dense_row out dense

  | Ok (`Sparse sparse) ->
    pr_sparse_row out sparse


let main input_path output_path incl_excl_spec_as_list header_only no_header =
  let inch =
    match input_path with
    | None -> stdin
    | Some path -> open_in path
  in

  let ouch =
    match output_path with
    | None -> stdout
    | Some path -> open_out path
  in

  let out = output_string ouch in

  match Cosovo.IO.of_channel ~no_header inch with
  | Error (`SyntaxError err) ->
    print_endline (Cosovo.IO.string_of_error_location err);
    exit 1

  | Error (`UnterminatedString line) ->
    Printf.printf "unterminated quote on line %d\n%!" line;
    exit 1

  | Error (`IntOverflow (line, offending_string)) ->
    Printf.printf "value %S on line %d cannot be represented as an integer\n%!"
      offending_string line;
    exit 1

  | Ok (header_opt, seq) ->

    let is_column_included =
      match incl_excl_spec_as_list, header_opt with
      | _ :: _, Some header ->
        included_columns_of_spec header incl_excl_spec_as_list
      | _, _ ->
        fun _ -> true
    in

    let header_sep =
      if header_only then
        "\n"
      else
        ","
    in

    (* print header *)
    (
      (
        match header_opt with
        | None -> ()
        | Some (`Dense dense) ->

          let _ = List.fold_left (
            fun (has_prev, j) column_name ->
              if is_column_included j then (
                if has_prev then
                  out header_sep;
                out column_name;
                true, j + 1
              )
              else
                has_prev, j + 1
          ) (false, 0) dense in
          ()

        | Some (`Sparse sparse) ->
          let _ = List.fold_left (
            fun has_prev (j, column_name) ->
              if is_column_included j then (
                if has_prev then
                  out header_sep;
                out column_name;
                true
              )
              else
                has_prev
          ) false sparse in
          ()
      );
      out "\n"
    );

    if not header_only then
      Seq.iter (pr_subset_of_columns is_column_included out) seq


open Cmdliner

let _ =
  let command =
    let doc = "echo a csv file, or a subset of its columns.  To select \
               a subset, provide an inclusion/exclusion specifier.  A \
               specifier starts with 'all' or 'none', and proceeds \
               with pairs: either 'i <regexp>' to include columns matching \
               a regexp, or 'x <regexp>' to exclude columns matching a \
               regexp." in

    let input_file_path =
      let doc = "path of the input csv file (if absent: stdin)" in
      Arg.(value & opt (some string) None &
           info ["i"; "input"] ~docv:"PATH" ~doc)
    in

    let output_file_path =
      let doc = "path of output csv file (if absent: stdout)" in
      Arg.(value & opt (some string) None &
           info ["o"; "output"] ~docv:"PATH" ~doc)
    in

    let header_only =
      let doc = "only echo the elements of the header, one per line" in
      Arg.(value & flag & info ["H";"header-only"] ~doc)
    in

    let no_header =
      let doc = "interpret the first line of the csv file as data, rather
                 than a header providing names for the fields in file" in
      Arg.(value & flag & info ["h";"no-header"] ~doc)
    in

    let incl_excl_cmds = Arg.(value & pos_all string [] & info []) in

    Term.(pure main
          $ input_file_path
          $ output_file_path
          $ incl_excl_cmds
          $ header_only
          $ no_header
         ), Term.info "csvcat" ~doc
  in
  match Term.eval ~catch:false command with
    | `Error _ -> exit 1
    | _ -> exit 0
