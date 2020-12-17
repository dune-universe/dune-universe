type t =
    {line: int; column: int}

type range = t * t

let line (p: t): int =
    p.line

let column (p:t): int =
    p.column

let start: t =
    {line = 0; column = 0}

let next_column (p: t): t =
    {p with column = p.column + 1}

let next_line (p: t): t =
    {line = p.line + 1; column = 0;}

let next (c: char) (p: t): t =
    if c = '\n' then
        next_line p
    else
        next_column p




module Print (PP: Pretty_printer.SIG) =
struct
    open Common

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
        let open PP in
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
        : PP.t
        =
        let start_line = line pos1
        and end_line   = line pos2
        and start_col  = column pos1
        and end_col    = column pos2
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
        let open PP in
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
        : PP.t
        =
        let start_line = line pos1
        and end_line   = line pos2
        and start_col  = column pos1
        and end_col    = column pos2
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
        let open PP in
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
