open Camyll.Highlight

let () =
  assert (validate_color "#000000" = Ok ());
  assert (validate_color "#999999" = Ok ());
  assert (validate_color "#123456" = Ok ());
  assert (validate_color "#123" = Ok ());
  assert (validate_color "#5555" = Ok ());
  assert (validate_color "#FfFf12A4" = Ok ())

let () =
  assert (Result.is_error (validate_color "#"));
  assert (Result.is_error (validate_color "#12345"));
  assert (Result.is_error (validate_color "#gggggg"))
