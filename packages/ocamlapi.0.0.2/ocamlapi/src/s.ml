open Core

(** Exceptions thrown while creating a Router.t *)
module Route_exceptions = struct

    exception DuplicateRouteTemplate of string

    exception InvalidRouteTemplate of string

    exception DuplicateHttpMethod of Cohttp.Code.meth
end

(** Module signature for http request/response bodies. *)
module type Body = sig
    type t

    val empty : t

    val of_string : string -> t
end

(** Module signature for async computations. *)
module type Io = sig
    type 'a t

    val return : 'a -> 'a t
end

(** Common types used by the router module signatures. *)
module type Router_types = sig
    
    (** Type of an http request. *)
    type req

    (** Type of an http message body.*)
    type body

    (** Type of an http response. *)
    type resp

    (** Type of an async computation. *)
    type 'a io

end

(** Configuration options for a module of type Router. *)
module type Router_config = sig
   
    include Router_types
    
    val default_exn_handler : ?vars:string Core.String.Table.t -> exn -> (resp * body) io
    
    val default_fallback : ?vars: string String.Table.t -> req -> body -> (resp * body) io 

end

(** Path-based routing for http requests. *)
module type Router = sig

    include Router_types

    (** A function of this type is called whenever a request matches a route.
        The vars argument is a table where the keys are the variable names declared in dynamic path segments,
        and the values are their values as appearing in the route that this request matched against. 
        
        For example, if a request made to the path ["/user/david"] matches against a route ["/user/<name>"], then the corresponding callback
        will be given a [vars] with a single key ["name"], and the associated value will be ["david"].
        *)
    type callback = ?vars: string String.Table.t -> req -> body -> (resp * body) io 
    
    (** A function of this type is called whenever a callback throws an exception. *)
    type exn_handler = ?vars:string Core.String.Table.t -> exn -> (resp * body) io
    
    (** Represents a url path with a list of supported http methods. *)
    type route = string * (Cohttp.Code.meth * callback) list

    (** If a module declares a list of routes named routes, it can be passed as a first-class value to the [create_from_modules] functions below. *)
    module type Routes = sig
        val routes : route list
    end

    (** If no exception handler is given while creating a router, [default_exn_handler] gets used. *)
    val default_exn_handler : exn_handler

    (** If no fallback callback is given while creating a router, [default_fallback] gets used. *)
    val default_fallback : callback

    (** The type of a request router. *)
    type t

    (** Create a router *)
    val create : ?exn_handler:exn_handler -> ?fallback_response: callback -> route list -> (t, exn) Result.t

    val create_exn : ?exn_handler: exn_handler -> ?fallback_response: callback -> route list -> t

    (** A convenience function to create a router from a list of modules, each of which declare a [routes] function. *)
    val create_from_modules : ?exn_handler:exn_handler -> ?fallback_response: callback -> (module Routes) list -> (t, exn) Result.t

    val create_from_modules_exn : ?exn_handler: exn_handler -> ?fallback_response: callback -> (module Routes) list -> t

    (** Dispatch a request to the appropriate route.
        If no matching route is found, [fallback_response] is called.
        If an exception is thrown when executing the callback registered to the matching route, [exn_handler] is called. *)
    val dispatch : t -> req -> body -> (resp * body) io

end
