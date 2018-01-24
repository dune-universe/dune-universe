let x = {qq|hello|qq}
let y w = qq$"hello %s"
let z w = qq${|hello ${w ^ w}|}
let s = {qq|answer = %d|qq} 42

