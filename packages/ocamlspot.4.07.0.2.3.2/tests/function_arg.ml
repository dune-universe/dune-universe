let (* function1 => *) function1 (* <= function1 *) (* fun arg x => *) x (* <= fun arg x *) = x (* ? fun arg x *)
let (* function2 => *) function2 (* <= function1 *) = fun (* fun arg x2 => *) x (* <= fun arg x2 *) -> x (* ? fun arg x2 *)
