let () =
  let f conn =
    Mindstorm.EV3.Sound.tone conn ~vol:2 ~freq:1000 ~ms:1000
  in
  Ev3_connect.(and_do { args = []; f })
