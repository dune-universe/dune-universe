module Format = struct
  include Format

  let array f ppf a = 
    let len = Array.length a in
      fprintf ppf "@[[| @[";
      Array.iteri (fun i v ->
        f ppf v;
        if i < len - 1 then fprintf ppf ";@ ")
        a;
      fprintf ppf "@] |]@]"
  
  let rec list (sep : (unit, formatter, unit) format)  f ppf = function
    | [] -> ()
    | [x] -> f ppf x
    | x::xs -> 
        fprintf ppf "@[%a@]%t%a" 
  	f x
  	(fun ppf -> fprintf ppf sep)
  	(list sep f) xs

  let opt f ppf = function
    | None -> fprintf ppf "None"
    | Some v -> f ppf v

  let option f ppf = function
    | None -> fprintf ppf "None"
    | Some v -> fprintf ppf "Some (%a)" f v
end

module List = struct
  include List

  let rec find_map_opt f = function
    | [] -> None
    | x::xs ->
        match f x with
        | Some v -> Some v
        | None -> find_map_opt f xs

end
