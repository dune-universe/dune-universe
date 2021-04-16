let roman_vals =
  [(900, "CM"); (500, "D"); (400, "CD"); (100, "C"); (100, "C"); (100, "C");
  (90, "XC"); (50, "L"); (40, "XL"); (10, "X"); (10, "X"); (10, "X");
  (9, "IX"); (5, "V"); (4, "IV"); (1, "I"); (1, "I"); (1, "I")]

let rec roman_recurse b n = function
  | [] -> ()
  | (n', s) :: t ->
      if n >= n' then
        begin
          Buffer.add_string b s;
          roman_recurse b (n - n') t
        end
      else
        roman_recurse b n t

let rec roman b n =
  if n < 1 then ()
  else if n > 999 then
    begin
      for _ = 1 to n / 1000 do Buffer.add_char b 'M' done;
      roman b (n mod 1000)
    end
  else
    roman_recurse b n roman_vals

let roman_string_of_int n =
  let b = Buffer.create 32 in
    roman b n;
    Buffer.contents b
