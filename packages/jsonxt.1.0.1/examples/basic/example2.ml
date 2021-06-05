let () =
  let json = Jsonxt.Basic.of_file "test.json" in
  Jsonxt.Basic.to_channel_hum stdout json;;
