let x1 = {fmt|hello world|fmt} (* CR jfuruse: it should be converted to simple string *)
let x2 = {fmt|hello %s world|fmt} 
let f1 y = {fmt|hello ${y} world|fmt}
let f2 y = {fmt|hello ${y} world|fmt} [@top]
let f3 y = fmt$ "hello${y} world"
let f4 y = fmt$ {|hello${y} world|}

