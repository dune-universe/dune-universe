open Ezjs_min

class type ['a] event = object
  method addListener : ('a -> unit) -> unit meth
  method removeListener : ('a -> unit) -> unit meth
  method hasListener : ('a -> unit) -> unit meth
end

class type ['a, 'b] event2 = object
  method addListener : ('a -> 'b -> unit) -> unit meth
  method removeListener : ('a -> 'b -> unit) -> unit meth
  method hasListener : ('a -> 'b -> unit) -> unit meth
end

class type ['a, 'b, 'c] event3 = object
  method addListener : ('a -> 'b -> 'c -> unit) -> unit meth
  method removeListener : ('a -> 'b -> 'c -> unit) -> unit meth
  method hasListener : ('a -> 'b -> 'c -> unit) -> unit meth
end

class type errorBrowser = object
  method message : js_string t prop
  method nomFichier : js_string t prop
  method numeroLigne : int prop
end

type error_browser = {
  error_message : string;
  error_fichier : string;
  error_ligne : int
}

let of_error_browser {error_message; error_fichier; error_ligne} =
  let o : errorBrowser t = Unsafe.obj [||] in
  o##.message := string error_message;
  o##.nomFichier := string error_fichier;
  o##.numeroLigne := error_ligne;
  o

let to_error_browser (o:errorBrowser t) = {
  error_message = to_string o##.message;
  error_fichier = to_string o##.nomFichier;
  error_ligne = o##.numeroLigne
}

let addListener1 (e:'a event t) f = e##addListener f
let addListener2 (e:('a, 'b) event2 t) f = e##addListener f
let addListener3 (e:('a, 'b, 'c) event3 t) f = e##addListener f
let removeListener1 (e:'a event t) f = e##removeListener f
let removeListener2 (e:('a, 'b) event2 t) f = e##removeListener f
let removeListener3 (e:('a, 'b, 'c) event3 t) f = e##removeListener f
let hasListener1 (e:'a event t) f = e##hasListener f
let hasListener2 (e:('a, 'b) event2 t) f = e##hasListener f
let hasListener3 (e:('a, 'b, 'c) event3 t) f = e##hasListener f
