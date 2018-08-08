class type t = object
  method m : int
  method n : int
end

class c x : t = object
  method m = x + 1
  (* method n = 42 *)
end
