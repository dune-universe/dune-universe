open Fmlib
open Common


type pos = Position.t
type range = pos * pos



module Make (PP: Pretty_printer.SIG) =
struct
    open PP

    let print_error_header
        (error_type:string)
        : t
        =
       (* Print the error header. *)
       let err = " ERROR " in
       let nfill =
         max 0 (80 - 3 - (String.length error_type + String.length err))
       in
       chain [fill 2 '-';
              char ' ';
              string error_type;
              string err;
              fill nfill '-';
              cut;
              cut]



    let print_tabs (offset: int) (tabs: int list): t =
        let tab_marker = char '^'
        and offset = fill offset ' '
        in
        let rec print number col lst =
            match lst with
            | [] ->
                empty, empty
            | [pos] ->
                let fl = fill (pos - col) ' ' in
                fl <+> tab_marker,
                fl <+> string (string_of_int number)
            | pos1 :: pos2 :: rest ->
                let fl = fill (pos1 - col) ' ' in
                let markers, numbers =
                    print (number + 1) (pos1 + 1) (pos2 :: rest)
                in
                fl <+> tab_marker <+> markers,
                fl <+> string (string_of_int number) <+> numbers
        in
        let markers, numbers = print 0 0 tabs
        in
        offset <+> markers <+> cut
        <+>
        offset <+> numbers <+> cut


    let maybe_print_tabs (offset: int) (tabs: int list): t =
        if tabs = [] then
            empty
        else
            cut <+> print_tabs offset tabs


    let print_line_sub
        (number_width: int)
        (line_number: int)
        (start: int)
        (beyond: int)
        (source: string)
        : PP.t
        =
        let line_number_str = string_of_int (line_number + 1)
        in
        let len = String.length line_number_str
        in
        assert (len <= number_width);
        fill (number_width - len) ' '
        <+> string line_number_str
        <+> string "| "
        <+> substring source start (beyond - start)
        <+> cut


    let print_line
        (number_width: int)
        (line_number: int)
        (line: string)
        : PP.t
        =
        print_line_sub
            number_width
            line_number
            0
            (String.length line)
            line


    let print_source
        (source: string)
        ((pos1,pos2): range)
        (error_tabs: int list)
        : PP.t
        =
        let start_line = Position.line pos1
        and end_line   = Position.line pos2
        and start_col  = Position.column pos1
        and end_col    = Position.column pos2
        and len        = String.length source
        in
        let end_col =
            if start_line = end_line && start_col = end_col then
                end_col + 1
            else
                end_col
        in
        assert (start_line <= end_line);
        assert (start_line < end_line || start_col < end_col);
        let number_width =
            String.length (string_of_int (end_line + 1))
        in
        let print_line start beyond line_no =
            print_line_sub number_width line_no start beyond source
        and skip_line_no =
            fill (number_width + 2) ' '
        in
        let rec print (char_offset: int) (line_no: int): t =
            if end_line < line_no || len < char_offset then
                empty
            else
                let pos_newline =
                    String.find (fun c -> c = '\n') char_offset source
                in
                (
                    if
                        line_no = start_line && start_line = end_line
                    then
                        maybe_print_tabs
                            (number_width + 2)
                            error_tabs
                        <+>
                        print_line char_offset pos_newline line_no
                        <+> skip_line_no
                        <+> fill start_col ' '
                        <+> fill (end_col - start_col) '^'
                        <+> cut

                    else if
                        line_no = start_line && start_line < end_line
                    then
                        skip_line_no
                        <+> fill start_col ' '
                        <+> char 'v'
                        <+> fill 20 '.'
                        <+> cut
                        <+> print_line char_offset pos_newline line_no

                    else if
                        line_no = end_line && start_line <> end_line
                    then
                        print_line char_offset pos_newline line_no
                        <+> skip_line_no
                        <+> fill (max 0 (end_col - 1)) '.'
                        <+> char '^'
                        <+> cut

                    else
                       (* normal line *)
                       print_line char_offset pos_newline line_no
                )
                <+> print (pos_newline + 1) (line_no + 1)
        in
        print 0 0



    let print_source_lines
        (lines: string Sequence.t)
        ((pos1, pos2): range)
        (error_tabs: int list)
        : PP.t
        =
        let start_line = Position.line pos1
        and end_line   = Position.line pos2
        and start_col  = Position.column pos1
        and end_col    = Position.column pos2
        in
        let end_col =
            if start_line = end_line && start_col = end_col then
                end_col + 1
            else
                end_col
        and start_line0 = max 0 (start_line - 5)
        in
        assert (start_line <= end_line);
        assert (start_line < end_line || start_col < end_col);
        assert (end_line < Sequence.length lines);
        let number_width =
            String.length (string_of_int (end_line + 1))
        in
        let skip_line_no =
            fill (number_width + 2) ' '
        in
        let print_lines first last =
            Interval.fold
                empty
                (fun i pr ->
                    pr
                    <+>
                    print_line
                        number_width
                        i
                        (Sequence.elem i lines))
                first (last + 1)
        in
        print_lines start_line0 (start_line - 1)
        <+>
        (
            if start_line = end_line then
                maybe_print_tabs
                    (number_width + 2)
                    error_tabs
                <+>
                print_lines start_line start_line
                <+>
                (
                    skip_line_no <+> fill start_col ' '
                    <+> fill (end_col - start_col) '^'
                    <+> cut
                )
            else
                (
                    skip_line_no <+> fill start_col ' '
                    <+> char 'v' <+> fill 20 '.' <+> cut
                )
                <+> print_lines start_line end_line
                <+>
                (
                    skip_line_no
                    <+> fill (max 0 (end_col - 1)) '.'
                    <+> char '^'
                    <+> cut
                )
        )
end

