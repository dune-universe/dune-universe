

(* Type for log levels *)
type log_level = Easy_logging_types.level
               [@@deriving show { with_path = false }]
module type HandlersT = Easy_logging_types.HandlersT
                     
                      
(** Makes a logging module from a Handlers module *)
module MakeLogging (H : HandlersT) =
  struct

    (** logger class *)
    class logger
            (name: string)
            (levelo: log_level option)
            (handlers_desc : H.desc list)  =
    object(self)

      (** Handlers associated to the logger *)
      val mutable handlers = List.map H.make handlers_desc

      (** optional level of the logger *)
      val mutable levelo = levelo

      (** Name of the logger *)
      val name = name

 
      method private log_msg tags msg_level msg =
        match levelo with
        | None ->()
        | Some level ->
           if msg_level >= level
           then
             begin
               let item : H.log_item= {
                   level = msg_level;
                   logger_name = name;
                   msg = msg;
                   tags=tags} in 
               List.iter (fun handler ->
                   H.apply handler item)
                 handlers
             end
           else
             ()                           
          
      method private log_msg_lazy tags (msg_level : log_level) msg =
        match levelo with
        | None ->()
        | Some level ->
           if msg_level >= level
           then
             begin
               let item : H.log_item = {
                   level = msg_level;
                   logger_name = name;
                   msg = Lazy.force msg;
                   tags= tags} in 
               List.iter (fun handler ->
                   H.apply handler item)
                 handlers
             end
           else
             ()

      method add_handler h = handlers <- h::handlers
      method set_level new_levelo =
        levelo <- new_levelo
        
      method flash ?tags:(tags=[]) = self#log_msg tags Flash
      method error ?tags:(tags=[]) = self#log_msg tags Error
      method warning ?tags:(tags=[]) = self#log_msg tags Warning
      method info ?tags:(tags=[]) =  self#log_msg tags Info
      method debug ?tags:(tags=[]) = self#log_msg tags Debug
                   
      method lflash ?tags:(tags=[]) = self#log_msg_lazy tags Flash
      method lerror ?tags:(tags=[]) = self#log_msg_lazy tags Error
      method lwarning ?tags:(tags=[]) = self#log_msg_lazy tags Warning
      method linfo ?tags:(tags=[]) =  self#log_msg_lazy tags Info
      method ldebug ?tags:(tags=[]) = self#log_msg_lazy tags Debug
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
        let l = new logger name None [] in
        Hashtbl.add _loggers name l;
        l
        
    let make_logger name lvl hdescs  =
      let l = new logger name lvl hdescs in
      Hashtbl.add _loggers name l;
      l
      
      
    let dummy = make_logger "dummy" None []
    
                    
  end

module Default_handlers = Default_handlers
                
(** Instantiation of [MakeLogging] over [Default_handlers] *)
module Logging = MakeLogging(Default_handlers)


