open Ezjs_min

class type mutedInfo = object
  method extensionId : js_string t optdef prop
  method muted : bool t prop
  method reason : js_string t optdef prop
end

class type tab = object
  method active : bool t prop
  method attention : bool t optdef prop
  method audible : bool t optdef prop
  method autoDiscardable : bool t optdef prop
  method cookieStoreId : js_string t optdef prop
  method discarded : bool t optdef prop
  method favIconUrl : js_string t optdef prop
  method height : int optdef prop
  method hidden : bool t prop
  method highlighted : bool t prop
  method id : int optdef prop
  method incognito : bool t prop
  method index : int prop
  method isArticle : bool t prop
  method isInReaderMode : bool t prop
  method lastAccessed : float t prop
  method mutedInfo : mutedInfo t optdef prop
  method openerTabId : int optdef prop
  method pinned : bool t prop
  method selected : bool t prop
  method sessionId : js_string t optdef prop
  method status : js_string t optdef prop
  method successorId : int optdef prop
  method title : js_string t optdef prop
  method url : js_string t optdef prop
  method width : int optdef prop
  method windowId : int prop
end

class type queryInfo = object
  method active : bool t optdef prop
  method audible : bool t optdef prop
  method autoDiscardable : bool t optdef prop
  method currentWindow : bool t optdef prop
  method discarded : bool t optdef prop
  method highlighted : bool t optdef prop
  method index : int optdef prop
  method lastFocusedWindow : bool t optdef prop
  method muted : bool t optdef prop
  method pinned : bool t optdef prop
  method status : js_string t optdef prop
  method title : js_string t optdef prop
  method url : js_string t optdef prop
  method windowId : int optdef prop
  method windowType : js_string t optdef prop
end

class type updateProperties = object
  method active : bool t optdef prop
  method autoDiscardable : bool t optdef prop
  method highlighted : bool t optdef prop
  method muted : bool t optdef prop
  method openerTabId : int optdef prop
  method pinned : bool t optdef prop
  method selected : bool t optdef prop
  method url : js_string t optdef prop
end

class type createProperties = object
  method active : bool t optdef prop
  method index : int optdef prop
  method openerTabId : int optdef prop
  method pinned : bool t optdef prop
  method selected : bool t optdef prop
  method url : js_string t optdef prop
  method windowId : int optdef prop
end

class type imageDetails = object
  method format : js_string t optdef prop
  method quality : int optdef prop
end

class type details = object
  method allFrames : bool t optdef prop
  method code : js_string t optdef prop
  method cssOrigin : js_string t optdef prop
  method file : js_string t optdef prop
  method frameId : int optdef prop
  method matchAboutBlank : bool t optdef prop
  method runAt : js_string t optdef prop
end

class type moveProperties = object
  method windowId : int optdef prop
  method index : int prop
end

class type pageSettings = object
  method edgeBottom : int optdef prop
  method edgeLeft : int optdef prop
  method edgeRight : int optdef prop
  method edgeTop : int optdef prop
  method footerCenter : js_string t optdef prop
  method footerLeft : js_string t optdef prop
  method footerRight : js_string t optdef prop
  method headerCenter : js_string t optdef prop
  method headerLeft : js_string t optdef prop
  method headerRight : js_string t optdef prop
  method marginBottom : int optdef prop
  method marginLeft : int optdef prop
  method marginRight : int optdef prop
  method marginTop : int optdef prop
  method orientation : int optdef prop
  method paperHeight : int optdef prop
  method paperSizeUnit : int optdef prop
  method paperWIdth : int optdef prop
  method scaling : int optdef prop
  method showBackgroundColors : bool t optdef prop
  method showBackgroundImages : bool t optdef prop
  method shrinkToFit : bool t optdef prop
end

class type zoomSettings = object
  method defaultZoomFactor : int optdef prop
  method mode : js_string t optdef prop
  method scope : js_string t optdef prop
end

class type highlightInfo = object
  method windowId : int optdef prop
  method tabs : int optdef prop
  method tabs_arr : int js_array t optdef prop
  method populate : bool optdef prop
end

class type reloadProperties = object
  method bypassCache: bool t optdef prop
end

class type moveInSuccessionOptions = object
  method append: bool t optdef prop
  method insert: bool t optdef prop
end

let make_script_details
    ?allFrames ?code ?cssOrigin ?file ?frameId ?matchAboutBlank ?runAt () =
  let details : details t = Unsafe.obj [||] in
  details##.allFrames := optdef bool allFrames;
  details##.code := optdef string code;
  details##.cssOrigin := optdef string cssOrigin;
  details##.file := optdef string file;
  details##.frameId := Optdef.option frameId;
  details##.matchAboutBlank := optdef bool matchAboutBlank;
  details##.runAt := optdef string runAt;
  details

let make_query
    ?active ?audible ?autoDiscardable ?currentWindow ?discarded ?highlighted
    ?index ?lastFocusedWindow ?muted ?pinned ?status ?title ?url ?windowId
    ?windowType () =
  let query : queryInfo t = Unsafe.obj [||] in
  query##.active := optdef bool active;
  query##.audible := optdef bool audible;
  query##.autoDiscardable := optdef bool autoDiscardable;
  query##.currentWindow := optdef bool currentWindow;
  query##.discarded := optdef bool discarded;
  query##.highlighted := optdef bool highlighted;
  query##.index := Optdef.option index;
  query##.lastFocusedWindow := optdef bool lastFocusedWindow;
  query##.muted := optdef bool muted;
  query##.pinned := optdef bool pinned;
  query##.status := optdef string status;
  query##.title := optdef string title;
  query##.url := optdef string url;
  query##.windowId := Optdef.option windowId;
  query##.windowType := optdef string windowType;
  query
