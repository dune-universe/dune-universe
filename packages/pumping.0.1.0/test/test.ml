type t = [%re? `A, Star (`B | `C)] ;;

(* Success *)
(`A `End : t) ;;
(`A (`B `End) : t) ;;
(`A (`B (`B `End)) : t) ;;
(`A (`C (`B `End)) : t) ;;

(* Failure *)
(`A : t) ;;
(`B `End : t) ;;
(`A (`A `End) : t) ;;
(`A (`C (`A `End)) : t) ;;


(* pattern syntaxes *)
(`A `End : [%re? 'A'..'Z']);;
(`C `End : [%re? Inter ((`A | `C),(`B | `C))]);;
(`C `End : [%re? (`A | `C) :: (`B | `C)]);;
(`End : [%re? Epsilon]);;

(* posix *)
(`a `End :[%re"a"]) ;;
(`abc `End :[%re"abc"]) ;;

(`a `End :[%re"[ab]"]) ;;
(`c `End :[%re"[ab]"]) ;; (*fail*)

(`a `End :[%re"[a-d]"]) ;;
(`x `End :[%re"[a-d]"]) ;; (*fail*)

(`a (`a (`a `End)) :[%re"a*"]) ;;
(`End :[%re"a*"]) ;;

(`a (`a (`a `End)) :[%re"a+"]) ;;
(`End :[%re"a+"]) ;; (*fail*)

(`a (`a (`a `End)) :[%re"a{3,}"]) ;;
(`a (`a (`a (`a `End))) :[%re"a{3,}"]) ;;
(`a (`a `End) :[%re"a{3,}"]) ;; (*fail*)

(`a (`a (`a `End)) :[%re"a{3,4}"]) ;;
(`a (`a (`a (`a `End))) :[%re"a{3,4}"]) ;;
(`a (`a `End) :[%re"a{3,4}"]) ;; (*fail*)
(`a (`a (`a (`a (`a `End)))) :[%re"a{3,4}"]) ;; (*fail*)

(`b `End :[%re"[ab]&[bc]"]) ;;
(`a `End :[%re"[ab]&[bc]"]) ;; (*fail*)


(* equivalences *)
let id x = x ;;
(id : [%re"aa*a"] -> [%re"a{2,}"]);;
(id : [%re"[ab]&[bc]"] -> [%re"b"]);;
(id : [%re"(a|b)(c|d)"] -> [%re"[a]c|[a]d|[b]c|[b]d"]);;

let f x = (x : [%re"a*"] :> [%re"[ab]*"]) ;;
