let string_of_charlist cl =
    let b = Bytes.create (List.length cl) in
    let rec implode i = function
    | []        -> ()
    | c :: cs   -> (Bytes.set b i c; implode (i + 1) cs)
    in implode 0 cl;
    Bytes.unsafe_to_string b
;; 

let charlist_of_string str =
    let rec explode i l =
        if i < 0 then l else
        explode (i - 1) (str.[i] :: l)
    in explode (String.length str - 1) []
;;
