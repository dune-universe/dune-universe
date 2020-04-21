open Container

type 'a t = {mutable flag:bool;
             first:  'a Seq.t;
             second: 'a Seq.t}

let empty () = {flag = false; first = Seq.empty (); second = Seq.empty ()}

let count_first (s: 'a t): int = Seq.count s.first

let count_second(s: 'a t): int = Seq.count s.second

let count (s: 'a t): int = count_first s + count_second s

let elem (i:int) (s: 'a t): 'a =
  assert (i < count s);
  let cnt0 = count_first s in
  if i < cnt0 then Seq.elem i s.first
  else Seq.elem (i-cnt0) s.second

let push (elem:'a) (s:'a t): unit =
  if not s.flag then begin
    assert (count_second s = 0);
    Seq.push elem s.first
  end else begin
    Seq.push elem s.second
  end

let clone (s:'a t): 'a t =
  {flag = true; first = s.first; second = Seq.copy s.second}
