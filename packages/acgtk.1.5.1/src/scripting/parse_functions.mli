open AcgData.Environment
open Functions

type 'a result =
  | Stop of 'a
  | Continue of 'a
              

   
val parse_file : string -> Functions.context -> Environment.t -> Functions.context * Environment.t
                                                                                                                      
val parse_entry : resize:bool -> in_channel -> Functions.context -> Environment.t -> (Functions.context * Environment.t) result
