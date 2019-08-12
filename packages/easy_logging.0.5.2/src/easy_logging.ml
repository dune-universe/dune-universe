
                      
module MakeLogging (H : Easy_logging_types.HandlersT) =
struct

  include Easy_logging_types
  
  let debug = ref false
      
  class logger
      ?parent:(parent=None)
      (name: string)
    =
    object(self)
          
      val name = name
      val mutable level : level option = None
      val mutable handlers : H.t list = []
      val parent : logger option = parent
      val mutable propagate = true

      val mutable tag_generators : (unit -> H.tag) list = []
                            
      method set_level new_level =
        level <- Some new_level
      method add_handler h = handlers <- h::handlers

      method get_handlers = handlers
      method set_handlers hs = handlers <- hs

      method set_propagate p = propagate <- p
           
      method effective_level : level =
        match level, parent  with
        | None, None  -> NoLevel
        | None, Some p -> p#effective_level
        | Some l,_ -> l
                          
      method get_handlers_propagate =
        if !debug
        then
          print_endline (Printf.sprintf "[%s] returning (%i) handlers" name
            (List.length handlers));
        match propagate, parent with
        | true, Some p -> handlers @ p#get_handlers_propagate
        | _ -> handlers

      method add_tag_generator t  =
        tag_generators <- t :: tag_generators

      method private treat_msg : 'a. ('a -> string) -> H.tag list -> level -> 'a -> unit
        = fun unwrap_fun tags msg_level msg ->
        
        if !debug
        then
          print_endline ( Printf.sprintf "[%s]/%s -- Treating msg \"%s\" at level %s"
                            name (match level with
                                  | None -> "None"
                                  | Some lvl -> (show_level lvl))
                            (unwrap_fun msg) (show_level msg_level));
        
        let generated_tags = List.map (fun x -> x ()) tag_generators in 
        let item : H.log_item= {
            level = msg_level;
            logger_name = name;
            msg = unwrap_fun msg;
            tags=generated_tags @ tags} in 
        List.iter (fun handler ->
            H.apply handler item)
          self#get_handlers_propagate
        
      method private _log_msg : 'a. ('a -> string) -> H.tag list -> level -> 'a -> unit
        = fun unwrap_fun tags msg_level msg ->
          if msg_level >= self#effective_level
          then
            self#treat_msg unwrap_fun tags msg_level msg
          else
            ()                           
            
      method private _flog_msg : 'a. H.tag list -> level -> ('a, unit, string, unit) format4 -> 'a
        =  fun tags msg_level -> 
        if msg_level >= self#effective_level
        then
          Printf.ksprintf (
              self#treat_msg (fun x -> x) tags msg_level)
        else Printf.ifprintf () 
        
        
      method flash : 'a. ?tags:H.tag list -> ('a, unit, string, unit) format4 -> 'a
        = fun ?tags:(tags=[]) -> self#_flog_msg tags Flash
      method error : 'a. ?tags:H.tag list -> ('a, unit, string, unit) format4 -> 'a
        = fun ?tags:(tags=[]) -> self#_flog_msg tags Error
      method warning : 'a. ?tags:H.tag list -> ('a, unit, string, unit) format4 -> 'a
        = fun ?tags:(tags=[]) -> self#_flog_msg tags Warning
      method info : 'a. ?tags:H.tag list -> ('a, unit, string, unit) format4 -> 'a
        = fun ?tags:(tags=[]) -> self#_flog_msg tags Info        
      method trace : 'a. ?tags:H.tag list -> ('a, unit, string, unit) format4 -> 'a
        = fun ?tags:(tags=[]) -> self#_flog_msg tags Trace
      method debug : 'a. ?tags:H.tag list -> ('a, unit, string, unit) format4 -> 'a
        = fun ?tags:(tags=[]) -> self#_flog_msg tags Debug
                               
                               
      method lflash ?tags:(tags=[]) = self#_log_msg Lazy.force tags Flash
      method lerror ?tags:(tags=[]) = self#_log_msg Lazy.force tags Error
      method lwarning ?tags:(tags=[]) = self#_log_msg Lazy.force tags Warning
      method linfo ?tags:(tags=[]) =  self#_log_msg Lazy.force tags Info
      method ltrace ?tags:(tags=[]) =  self#_log_msg Lazy.force tags Trace
      method ldebug ?tags:(tags=[]) = self#_log_msg Lazy.force tags Debug
                                    
      method sflash ?tags:(tags=[]) = self#_log_msg (fun x -> x) tags Flash
      method serror ?tags:(tags=[]) = self#_log_msg (fun x -> x) tags Error
      method swarning ?tags:(tags=[]) = self#_log_msg (fun x -> x) tags Warning
      method sinfo ?tags:(tags=[]) =  self#_log_msg (fun x -> x) tags Info
      method strace ?tags:(tags=[]) =  self#_log_msg (fun x -> x) tags Trace
      method sdebug ?tags:(tags=[]) = self#_log_msg (fun x -> x) tags Debug
    end

    let root_logger = new logger "root"
      
    module Infra =
      Logging_infra.MakeTree(
          struct
            type t = logger
            let make (n:string) parent = new logger ~parent n
            let root = root_logger
          end)
      
      
    let get_logger name =
      Infra.get name
      
    let make_logger ?propagate:(propagate=true) name lvl hdescs  =
      let l = Infra.get name in
      l#set_level lvl;
      l#set_propagate propagate;
      List.iter (fun hdesc -> l#add_handler (H.make hdesc)) hdescs;
      l

    let set_debug v =
      debug := v

   end

module Handlers = Handlers
                
module Logging = MakeLogging(Handlers)


