let () =
  Markup.channel stdin
  |> Plist_xml.parse_exn
  |> Plist_xml.signals
  |> Markup.pretty_print
  |> Markup.write_xml
  |> Markup.to_channel stdout;
