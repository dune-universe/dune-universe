
type wday = Sun | Mon | Tue | Wed | Thu | Fri | Sat

let string_of_wday = function
  | Sun -> "Sun" | Mon -> "Mon" | Tue -> "Tue" | Wed -> "Wed"
  | Thu -> "Thu" | Fri -> "Fri" | Sat -> "Sat"

type month =
  Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec

let index_of_month = function
  | Jan -> 0  | Feb -> 1  | Mar -> 2  | Apr -> 3  | May -> 4  | Jun -> 5
  | Jul -> 6  | Aug -> 7  | Sep -> 8  | Oct -> 9  | Nov -> 10 | Dec -> 11

let string_of_month = function
  | Jan -> "Jan"  | Feb -> "Feb"  | Mar -> "Mar"  | Apr -> "Apr"
  | May -> "May"  | Jun -> "Jun"  | Jul -> "Jul"  | Aug -> "Aug"
  | Sep -> "Sep"  | Oct -> "Oct"  | Nov -> "Nov"  | Dec -> "Dec"

(* of_m.(m) = ⌊13(m+1)/5⌋ mod 7 *)
let of_m = [| 1; 4; 3; 6; 1; 4;  6; 2; 5; 0; 3; 5 |]

(* of_y.(y) = y + ⌊y/4⌋  mod 7, for y = 0,...,27 *)
let of_y = [| 0; 1; 2; 3;  5; 6; 0; 1;  3; 4; 5; 6;  1; 2; 3; 4;
              6; 0; 1; 2;  4; 5; 6; 0;  2; 3; 4; 5 |]

(* of_c.(c) = ⌊c/4⌋ - 2c  mod 7, for c = 0,...,3 *)
let of_c = [| 0; 5; 3; 1 |]

(* Zeller's algorithm *)
let compute_wday day (* ∈ {1,...,31} *) month year =
  let year = if month = Jan || month = Feb then year - 1 else year in
  let y = (year mod 100) mod 28 in
  let y = of_y.(if y < 0 then 28 + y else y) in
  let c = (year / 100) mod 4 in
  let c = of_c.(if c < 0 then c + 4 else c) in
  match (day + of_m.(index_of_month month) + y + c) mod 7 with
  | 1 -> Sun
  | 2 -> Mon
  | 3 -> Tue
  | 4 -> Wed
  | 5 -> Thu
  | 6 -> Fri
  | 0 -> Sat
  | _ -> assert false

type t = { day: int;
           month: month;
           year: int;
           hour: int;
           min: int;
           sec: int;
           tz: int;
           wday: wday Lazy.t;
         }

let day d = d.day
let month d = d.month
let year d = d.year
let hour d = d.hour
let min d = d.min
let sec d = d.sec
let wday d = Lazy.force d.wday

let month_of_string s ofs =
  match s.[ofs] with
  | 'J' -> if s.[ofs+1] = 'a' then Jan
          else if s.[ofs+2] = 'n' then Jun
          else Jul
  | 'F' -> Feb
  | 'M' -> if s.[ofs+2] = 'r' then Mar else May
  | 'A' -> if s.[ofs+1] = 'p' then Apr else Aug
  | 'S' -> Sep
  | 'O' -> Oct
  | 'N' -> Nov
  | 'D' -> Dec
  | _ -> assert false

(* When the parsing fails, return the Epoch *)
let epoch = { day = 1;  month = Jan;  year = 1970;  wday = lazy Thu;
              hour = 0;  min = 0;  sec = 0;  tz = 0 }

(* Parse date of the form [s].  Never fail.  Simple minded. *)
(* FIXME: return an option instead?? *)
(* FIXME: one should validate the date *)
let of_string s =
  try
    let day = int_of_string(String.sub s 5 2) in
    let month = month_of_string s 8 in
    let year = int_of_string(String.sub s 12 4) in
    let hour = int_of_string(String.sub s 17 2) in
    let min = int_of_string(String.sub s 20 2) in
    let sec = int_of_string(String.sub s 23 2) in
    let tz = if s.[26] = '+' then int_of_string(String.sub s 27 4)
             else int_of_string(String.sub s 26 5) in
    { day; month; year;  hour; min; sec;  tz;
      wday = lazy(compute_wday day month year) }
  with _ -> epoch

let to_string d =
  let wday = string_of_wday(wday d) in
  let month = string_of_month d.month in
  Printf.sprintf "%s, %d %s %d %d:%d:%d %+05d"
                 wday d.day month d.year d.hour d.min d.sec d.tz
