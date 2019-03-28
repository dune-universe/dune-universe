

(* Type for log levels *)
type log_level = Easy_logging_types.level
               [@@deriving show { with_path = false }]
module type HandlersT = Easy_logging_types.HandlersT
                     
                      
                      
module MakeLogging (H : HandlersT) =
  struct

  
    class logger
            (name: string)
            (level: log_level)
            (handlers_desc : H.desc list)  =
    object(self)

      val mutable handlers = List.map H.make handlers_desc

      val mutable level = level

      val name = name
 
      method add_handler h = handlers <- h::handlers
      method set_level new_level =
        level <- new_level

      method private _log_msg : 'a. ('a -> string) -> H.tag list -> log_level -> 'a -> unit
        = fun unwrap_fun tags msg_level msg ->
           if msg_level >= level
           then
             begin
               let item : H.log_item= {
                   level = msg_level;
                   logger_name = name;
                   msg = unwrap_fun msg;
                   tags=tags} in 
               List.iter (fun handler ->
                   H.apply handler item)
                 handlers
             end
           else
             ()                           
          

      method private _flog_msg : 'a. H.tag list -> log_level -> ('a, unit, string, unit) format4 -> 'a
        =  fun tags msg_level -> 
        if msg_level >= level
        then
          Printf.ksprintf (
              fun msg -> 
              let item : H.log_item = {
                  level = msg_level;
                  logger_name = name;
                  msg = msg;
                  tags= []} in 
              List.iter (fun handler ->
                  H.apply handler item)
                handlers)
        else Printf.ifprintf () 
        

      method flash : 'a. ?tags:H.tag list -> ('a, unit, string, unit) format4 -> 'a
        = fun ?tags:(tags=[]) -> self#_flog_msg tags Flash
      method error : 'a. ?tags:H.tag list -> ('a, unit, string, unit) format4 -> 'a
        = fun ?tags:(tags=[]) -> self#_flog_msg tags Error
      method warning : 'a. ?tags:H.tag list -> ('a, unit, string, unit) format4 -> 'a
        = fun ?tags:(tags=[]) -> self#_flog_msg tags Warning
      method info : 'a. ?tags:H.tag list -> ('a, unit, string, unit) format4 -> 'a
        = fun ?tags:(tags=[]) -> self#_flog_msg tags Info        
      method debug : 'a. ?tags:H.tag list -> ('a, unit, string, unit) format4 -> 'a
        = fun ?tags:(tags=[]) -> self#_flog_msg tags Debug

                               
      method sflash ?tags:(tags=[]) = self#_log_msg (fun x->x) tags Flash
      method serror ?tags:(tags=[]) = self#_log_msg (fun x->x) tags Error
      method swarning ?tags:(tags=[]) = self#_log_msg (fun x->x) tags Warning
      method sinfo ?tags:(tags=[]) =  self#_log_msg (fun x->x) tags Info
      method sdebug ?tags:(tags=[]) = self#_log_msg (fun x->x) tags Debug

                                    
      method lflash ?tags:(tags=[]) = self#_log_msg Lazy.force tags Flash
      method lerror ?tags:(tags=[]) = self#_log_msg Lazy.force tags Error
      method lwarning ?tags:(tags=[]) = self#_log_msg Lazy.force tags Warning
      method linfo ?tags:(tags=[]) =  self#_log_msg Lazy.force tags Info
      method ldebug ?tags:(tags=[]) = self#_log_msg Lazy.force tags Debug
    end

  
    let _loggers : (string, logger) Hashtbl.t =  Hashtbl.create 10
                                               
    let set_level p lvlo =
      Hashtbl.iter
        (fun n l  ->
          
          if String.sub n 0 (String.length p) = p
          then
            l#set_level lvlo;)
        _loggers
      
    let get_logger name =
      if Hashtbl.mem _loggers name
      then
        Hashtbl.find _loggers name
      else
        let l = new logger name NoLevel [] in
        Hashtbl.add _loggers name l;
        l
        
    let make_logger name lvl hdescs  =
      let l = new logger name lvl hdescs in
      Hashtbl.add _loggers name l;
      l
      
    let dummy () = make_logger "dummy" NoLevel []
    
   end

module Default_handlers = Default_handlers
                
module Logging = MakeLogging(Default_handlers)


