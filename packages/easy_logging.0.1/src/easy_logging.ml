

(* Type for log levels *)
type log_level = Easy_logging_types.level
                                  
type log_item = Easy_logging_types.log_item
              
              
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

 
      method private log_msg msg_level msg =
        match levelo with
        | None ->()
        | Some level ->
           if msg_level >= level
           then
             begin
               let item : log_item= {
                   level = msg_level;
                   logger_name = name;
               msg = msg} in 
               List.iter (fun handler ->
                   H.apply handler item)
                 handlers
             end
           else
             ()                           
          
      method private log_msg_lazy (msg_level : log_level) msg =
        match levelo with
        | None ->()
        | Some level ->
           if msg_level >= level
           then
             begin
               let item : log_item = {
                   level = msg_level;
                   logger_name = name;
                   msg = Lazy.force msg} in 
               List.iter (fun handler ->
                   H.apply handler item)
                 handlers
             end
           else
             ()

      method add_handler h = handlers <- h::handlers
      method set_level new_levelo =
        levelo <- new_levelo
        
      method flash = self#log_msg Flash
      method error = self#log_msg Error
      method warning = self#log_msg Warning
      method info =  self#log_msg Info
      method debug = self#log_msg Debug
                   
      method lflash = self#log_msg_lazy Flash
      method lerror = self#log_msg_lazy Error
      method lwarning = self#log_msg_lazy Warning
      method linfo =  self#log_msg_lazy Info
      method ldebug = self#log_msg_lazy Debug
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
    module Handlers = H
                    
  end

module Default_handlers = Default_handlers
                
(** Instantiation of [MakeLogging] over [Default_handlers] *)
module Logging = MakeLogging(Default_handlers)


