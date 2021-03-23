module Make (Console : Mirage_console.S) = struct
  let log console fmt = Format.kasprintf (Console.log console) fmt

  let start console =
    let v = Io_page.get_order 15 in
    log console "addr: %nx." (Io_page.get_addr v)
end
