# ppx_matches

Translates

    [%matches? Some(2)]

into

    (function | Some(2) => true | _ => false) : int option -> bool