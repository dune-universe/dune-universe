open Ezjs_min

class type condition = object end
class type action = object end

class type pageUrl = object
  method hostContains : js_string t optdef prop
  method hostEquals : js_string t optdef prop
  method hostPrefix : js_string t optdef prop
  method hostSuffix : js_string t optdef prop
  method pathContains : js_string t optdef prop
  method pathPrefix : js_string t optdef prop
  method pathSuffix : js_string t optdef prop
  method queryContains : js_string t optdef prop
  method queryEquals : js_string t optdef prop
  method queryPrefix : js_string t optdef prop
  method querySuffix : js_string t optdef prop
  method urlContains : js_string t optdef prop
  method urlEquals : js_string t optdef prop
  method urlMatches : js_string t optdef prop
  method originAndPathMatches : js_string t optdef prop
  method urlPrefix : js_string t optdef prop
  method urlSuffix : js_string t optdef prop
  method schemes : js_string t js_array t optdef prop
  method ports : int js_array t optdef prop
end

class type pageStateMatcher = object
  method pageUrl : pageUrl t optdef prop
  method css : js_string t js_array t optdef prop
  method isBookmarked : bool t optdef prop
end

class type rule = object
  method id : js_string t optdef prop
  method priority : int optdef prop
  method conditions : condition t js_array t prop
  method actions : action t js_array t prop
end

class type declarativeEvent = object
  method addRules : rule t js_array t -> ('a t -> unit) callback -> unit meth
  method removeRules : js_string t js_array t optdef -> ('a t -> unit) callback -> unit meth
  method getRules : js_string t js_array t -> ('a t -> unit) callback -> unit meth
end

class type declarativeContent = object
  method _ShowPageAction : action t constr prop
  method _SetIcon : (js_string t -> action t) constr prop
  method _RequestContentScript :
    (js_string t js_array t optdef ->
     js_string t js_array t optdef -> bool t optdef -> bool t optdef -> action t) constr prop
  method _PageStateMatcher : (pageStateMatcher t -> condition t) constr prop
  method onPageChanged : declarativeEvent t prop
end

let declarativeContent : declarativeContent t = Unsafe.variable "chrome.declarativeContent"

let make_pageUrl
    ?hostContains ?hostEquals ?hostPrefix ?hostSuffix ?pathContains ?pathPrefix
    ?pathSuffix ?queryContains ?queryEquals ?queryPrefix ?querySuffix ?urlContains
    ?urlEquals ?urlMatches ?originAndPathMatches ?urlPrefix ?urlSuffix ?schemes
    ?ports () =
  let url : pageUrl t = Unsafe.obj [||] in
  url##.hostContains := optdef string hostContains;
  url##.hostEquals := optdef string hostEquals;
  url##.hostPrefix := optdef string hostPrefix;
  url##.hostSuffix := optdef string hostSuffix;
  url##.pathContains := optdef string pathContains;
  url##.pathPrefix := optdef string pathPrefix;
  url##.pathSuffix := optdef string pathSuffix;
  url##.queryContains := optdef string queryContains;
  url##.queryEquals := optdef string queryEquals;
  url##.queryPrefix := optdef string queryPrefix;
  url##.querySuffix := optdef string querySuffix;
  url##.urlContains := optdef string urlContains;
  url##.urlEquals := optdef string urlEquals;
  url##.urlMatches := optdef string urlMatches;
  url##.originAndPathMatches := optdef string originAndPathMatches;
  url##.urlPrefix := optdef string urlPrefix;
  url##.urlSuffix := optdef string urlSuffix;
  url##.schemes := optdef (of_listf string) schemes;
  url##.ports := Optdef.option ports;
  url

let make_pageStateMatcher ?pageUrl ?css ?isBookmarked () =
  let state_matcher : pageStateMatcher t = Unsafe.obj [||] in
  state_matcher##.pageUrl := Optdef.option pageUrl;
  state_matcher##.css := optdef (of_listf string) css;
  state_matcher##.isBookmarked := optdef bool isBookmarked;
  state_matcher

let make_condition_base state_matcher =
  let cst = declarativeContent##._PageStateMatcher in
  new%js cst state_matcher

let make_condition ?hostContains ?hostEquals ?hostPrefix ?hostSuffix ?pathContains ?pathPrefix
    ?pathSuffix ?queryContains ?queryEquals ?queryPrefix ?querySuffix ?urlContains
    ?urlEquals ?urlMatches ?originAndPathMatches ?urlPrefix ?urlSuffix ?schemes
    ?ports ?css ?isBookmarked () =
  let pageUrl = make_pageUrl ?hostContains ?hostEquals ?hostPrefix ?hostSuffix ?pathContains ?pathPrefix
    ?pathSuffix ?queryContains ?queryEquals ?queryPrefix ?querySuffix ?urlContains
    ?urlEquals ?urlMatches ?originAndPathMatches ?urlPrefix ?urlSuffix ?schemes
    ?ports () in
  let state_matcher = make_pageStateMatcher ~pageUrl ?css ?isBookmarked () in
  make_condition_base state_matcher

let showPageAction () =
  let cst = declarativeContent##._ShowPageAction in new%js cst
let setIcon path =
  let cst = declarativeContent##._SetIcon in
  new%js cst (string path)
let requestContentScript ?css ?js ?allFrames ?matchAboutBlank () =
  let cst = declarativeContent##._RequestContentScript in
  new%js cst (optdef (of_listf string) css) (optdef (of_listf string) js)
    (optdef bool allFrames) (optdef bool matchAboutBlank)

let make_rule ?id ?priority conditions actions =
  let rule : rule t = Unsafe.obj [||] in
  rule##.conditions := of_list conditions;
  rule##.actions := of_list actions;
  rule##.priority := Optdef.option priority;
  rule##.id := optdef string id;
  rule

let addRules rules f =
  declarativeContent##.onPageChanged##addRules (of_list rules) (wrap_callback f)

let removeRules ?ids f =
  declarativeContent##.onPageChanged##removeRules
    (optdef (of_listf string) ids) (wrap_callback f)
