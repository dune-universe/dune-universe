type pretty_printer = {
    pretty_printer_channel: out_channel;
    mutable pretty_printer_fresh_line: bool;
    mutable pretty_printer_tab_depth: int;
  }

let make_pretty_printer ch =
  { pretty_printer_channel = ch;
    pretty_printer_fresh_line = true;
    pretty_printer_tab_depth = 0; }

let output_spaces ch n =
  for _ = 1 to n do
    output_char ch ' '
  done

let pretty_printer_newline (p : pretty_printer) =
  output_string p.pretty_printer_channel "\n";
  p.pretty_printer_fresh_line <- true

let pretty_printer_tab (p : pretty_printer) =
  p.pretty_printer_tab_depth <- p.pretty_printer_tab_depth + 4

let pretty_printer_untab (p : pretty_printer) =
  p.pretty_printer_tab_depth <- p.pretty_printer_tab_depth - 4

let pretty_printer_print (p: pretty_printer) (s: string) =
  if p.pretty_printer_fresh_line then begin
    output_spaces p.pretty_printer_channel p.pretty_printer_tab_depth;
    p.pretty_printer_fresh_line <- false;
  end;
  output_string p.pretty_printer_channel s

let pretty_printer_println (p: pretty_printer) (s: string) =
  pretty_printer_print p s;
  pretty_printer_newline p

let pretty_printer_open_block p s =
  pretty_printer_println p s;
  pretty_printer_tab p

let pretty_printer_close_block p s =
  pretty_printer_untab p;
  pretty_printer_println p s
