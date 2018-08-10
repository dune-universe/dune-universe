type t = {
    id: Cstruct.t; 
    host: string;
    port: int
  } [@@deriving fields]



    
let to_string t =
  let id_str = Hex.of_cstruct t.id |> Hex.to_string in 
  Fmt.strf "%s@%s:%d" id_str t.host t.port


            
let of_string s =
  let at = Str.regexp "@" in
  let colon = Str.regexp ":" in
  
  let i_pos = Str.search_forward at s 0 in

  let id =
    Str.string_before s i_pos |> Hex.of_string |> Hex.to_cstruct
  in

  let hd :: tl = Str.string_after s i_pos |> (Str.split colon)  in

  let host, port = (hd, List.hd tl |> int_of_string) in
  {id; host; port}


    

let to_cstruct t = to_string t |> Cstruct.of_string
                                    
let of_cstruct cs = Cstruct.to_string cs |> of_string


let make ?id ~host ~port () =
  match id with

  | Some x -> {id=x; host; port}

  | None ->

     let s =
       Fmt.strf "%s:%d" host port |> Cstruct.of_string
     in 

     let nid = FNV1A.digest s in 

     {id = nid; host; port}

       

       
                           
                           
  
let compare l r =
  Cstruct.compare l.id r.id

    
  
