open General.Abbr

let a = [|1; 2; 3|]

let _ = a.(0) (* Array.get *)

let _ = a.(0) <- 1 (* Array.set *)

let s = "abc"

let _ = s.[0] (* String.get *)

let _ = s.[0] <- 'A' (* String.set *)

let b = By.of_string "abc"

let _ = b.[0] (* String.get *)

let _ = b.[0] <- 'A' (* String.set *)
