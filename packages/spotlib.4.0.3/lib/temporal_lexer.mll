{
  (** Date *)
  
  type date = {
    year : int;  (* 1998 *)
    month : int; (* 1-12 *)
    day : int
  }

  type time = {
    hour : int;
    min : int;
    sec : int
  }	
}

let digit = ['0'-'9']

rule parse_date = parse
  | (digit digit digit digit as year)
    '-'
    (digit digit as month)
    '-'
    (digit digit as day)
    {
      { year  = int_of_string year;
        month = int_of_string month;
        day   = int_of_string day 
      }
    }

(* CR jfuruse: 1:30:30,5 and 11:30.5 are not supported yet *)
and parse_time = parse
  | (digit digit as hour)
    ':'
    (digit digit as min)
    ':'
    (digit digit as sec)
    {
      { hour = int_of_string hour;
        min  = int_of_string min;
        sec  = int_of_string sec;
      }
    }
  | (digit digit as hour)
    ':'
    (digit digit as min)
    {
      { hour = int_of_string hour;
        min  = int_of_string min;
        sec  = 0;
      }
    }
  | (digit digit as hour)
    {
      { hour = int_of_string hour;
        min  = 0;
        sec  = 0;
      }
    }

and parse_t = parse
  | 'T' { () }

and parse_tzone = parse
  | 'Z' { `UTC }
  | (['+' '-'] as sign) (digit digit as hour) ':'? (digit digit as min) 
      { 
        let hour = int_of_string hour in
        let min = int_of_string min in
        match sign with
        | '+' -> `Plus (hour, min)
        | '-' -> `Minus (hour, min)
        | _ -> assert false
      }
  | (['+' '-'] as sign) (digit digit as hour)
      { 
        let hour = int_of_string hour in
        match sign with
        | '+' -> `Plus (hour, 0)
        | '-' -> `Minus (hour, 0)
        | _ -> assert false
      }

{
}
