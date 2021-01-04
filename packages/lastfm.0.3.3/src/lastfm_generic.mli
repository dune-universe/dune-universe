(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2010 Savonet team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details, fully stated in the COPYING
  file at the root of the liquidsoap distribution.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

(** API to various lastfm protocols (generic modules). *)

(** Records for login and client *)
type client = { client : string; version : string }

type login = { user : string; password : string }

(** This is the type of Http request API
  * that the modules require. *)
module type Http_t = sig
  type request = Get | Post of string

  exception Http of string

  val default_timeout : float ref

  val request :
    ?timeout:float ->
    ?headers:(string * string) list ->
    ?port:int ->
    host:string ->
    url:string ->
    request:request ->
    unit ->
    string
end

(** This is the type of the Audioscrobbler API. *)
module type Audioscrobbler_t = sig
  (** Audioscrobbler is the submission protocol as described at
    
     {{:http://www.audioscrobbler.net/development/protocol/}http://www.audioscrobbler.net/development/protocol/} *)

  (** {2 Types} *)

  (** source of the track *)
  type source = User | Broadcast | Recommendation | Lastfm | Unknown

  (** possible rating for the track *)
  type rating = Love | Ban | Skip

  (** type of action *)
  type action = NowPlaying | Submit

  (** song submission type *)
  type song = {
    artist : string;
    track : string;
    time : float option;
    source : source option;
    rating : rating option;
    length : float option;
    album : string option;
    trackauth : string option;
    tracknumber : int option;
    musicbrainzid : string option;
  }

  (** various errors *)
  type error =
    | Http of string
    | Banned
    | Badauth
    | Badtime
    | Failed of string
    | UnknownError of string
    | Success
    | Internal of string
    | BadData of string

  exception Error of error

  (** Get meaning of Error e *)
  val string_of_error : error -> string

  (** Base port. Default: 80 *)
  val base_port : int ref

  (** Base host. Default: "post.audioscrobbler.com" *)
  val base_host : string ref

  (** {2 Common API}

     Functions common to both basic and advanced APIs *)

  (** [get_song] 
        create a song record based on given values.

        Optional records can be ommited there. *)
  val get_song :
    ?time:float ->
    ?source:source ->
    ?rating:rating ->
    ?length:float ->
    ?album:string ->
    ?tracknumber:int ->
    ?musicbrainzid:string ->
    ?trackauth:string ->
    artist:string ->
    track:string ->
    unit ->
    song

  (** Check wether required song informations
        are supplied for given action.

        Raises [Error (BadData reason)] if invalid data is given.

        See protocol details there:

          {{:http://www.audioscrobbler.net/development/protocol/}http://www.audioscrobbler.net/development/protocol/}
     *)
  val check_song : song -> action -> unit

  (** {2 Basic API} 
     
     Using this API, all requests are done in one single step *)

  (** [do_np client login song]
     execute a nowplaying request 
     with authentification 

     Optional host parameter is a pair
     "host",port to override the global
     values.*)
  val do_np :
    ?timeout:float -> ?host:string * int -> client -> login -> song -> unit

  (** [do_submit client login songs]
      execute a nowplaying request 
      with authentification. 

     This functions returns a list
     songs for which supplied informations
     were incomplete, with corresponding exception 
     (see [check_song] source) *)
  val do_submit :
    ?timeout:float ->
    ?host:string * int ->
    client ->
    login ->
    song list ->
    (error * song) list

  (** {2 Advanced API} 
     
     This API is for advanced usages.
     
     You may use it this way:
     {ol {- handshake : initiate session }
      {- np/submit: execute request } }

    The module will cache session informations and avoid redundant
    requests, so you might always call handshake. 
    
    However, if a np or submit fails, it can be because
    the session has expired on server side, but the module cache
    still refered to it. So you might clear this session id, and
    try another handshake+submit/np.
    
    Check do_no and do_submit for examples. *)

  (** [handshake client login] 
     open session, returns session ID 
     
     Optional host parameter is a pair 
     "host",port to override the global
     values. *)
  val handshake :
    ?timeout:float -> ?host:string * int -> client -> login -> string

  (** [np sessionID track]
     execute a nowplaying request *)
  val np : ?timeout:float -> string -> song -> unit

  (** [submit sessionID tracks]
     execute a submit request 

     This functions returns a list
     songs for which supplied informations
     were incomplete, with corresponding exception 
    (see check_song) *)
  val submit : ?timeout:float -> string -> song list -> (error * song) list
end

(** This is the type of the Radio API. *)
module type Radio_t = sig
  (** API for using lastfm radios 

    No protocol documentation avaible for now... *)

  (** {2 Types} *)

  (** Type for track datas 
   
    A track is a list of "field","value" metadatas and an uri *)
  type track = (string * string) list * string

  (** Various errors *)
  type error =
    | Http of string
    | Auth of string
    | Adjust of string * string
    | Playlist
    | Empty

  exception Error of error

  (** Get meaning of Error e *)
  val string_of_error : error -> string

  (** Base host. Default: "ext.last.fm" *)
  val base_host : string ref

  (** {2 Basic API} *)

  (** [get uri] performs whole process and
      outputs a list of metadatas,uri
      from given lastfm uri.
      
      This function cannot handle well
      multiple anonymous requests.
      
      If you plan to play simultaneously 
      several anonymous radios, you better 
      use the advanced API to keep track 
      of every opened session. *)
  val get : ?timeout:float -> string -> track list

  (** {2 Advanced API} 

     Using this API you shall call: 
    {ol {- parse: get required parts of the uri}
    {- init: initiate a session}
    {- adjust: adjust station } }
    Then you can use any of the following:
    {ul {- playlist: return the raw xml content of the playlist}
    {- tracks : returns the parsed playlist.}}
    After each of those calls, you shall use *only* one
    of the songs from the playlist.
  
    The module will cache session informations and avoid redundant
    requests, so you might always call init and adjust.

    If you call [playlist], and anything went bad, 
    you have to call [clear] to remove cached data 
    about this session. 
    
    In any case you may also give another try, 
    in case of inconsistent cached session data. 
    See [get] source for details *)

  (** [parse uri] parse the given lastfm:// uri
    *
    * returns login,station,options *)
  val parse : string -> login * string * string option

  (** [init login] initiate lastfm session
    *
    * Returns the session id *)
  val init : ?timeout:float -> login -> string

  (** [adjust id station] adjusts lastfm station 
    * for given session ID 
    *
    * Returns a list of (variable,value) as returned
    * by the server. Contains settings for adjusted
    * radio.
    *)
  val adjust : ?timeout:float -> string -> string -> (string * string) list

  (** [playlist id] returns the raw xml content of the playlist *)
  val playlist : ?timeout:float -> string -> string option -> string

  (** [tracks id] 
    * returns a list of metadatas,uri  *)
  val tracks : ?timeout:float -> string -> string option -> track list

  (** [clear id] closes and clear all 
    * informations about the given session ID *)
  val clear : string -> unit
end

(** Generic implementation of Audioscrobbler, independent 
  * from the Http request. *)
module Audioscrobbler_generic (Http : Http_t) : Audioscrobbler_t

(** Generic implementation of the Radio API, independant
  * from the Http request. *)
module Radio_generic (Http : Http_t) : Radio_t
