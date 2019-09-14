(** Binding to the Dropbox
    {{:https://www.dropbox.com/developers/core/docs}Remote API}. *)

type error_description = { error: string;
                           error_description: string }

type error =
  | Invalid_arg of error_description
  (** Bad input parameter.  The string should indicate why. *)
  | Invalid_token of error_description
  (** Bad or expired token. This can happen if the user or Dropbox
      revoked or expired an access token. To fix, you should
      re-authenticate the user. *)
  | Invalid_oauth of error_description
  (** Bad OAuth request (wrong consumer key, bad nonce, expired
      timestamp...). Unfortunately, re-authenticating the user won't
      help here. *)
  | Conflict of error_description
  (** A conflict occured when uploading a file.  See {!S.files_put}. *)
  | Too_many_requests of error_description
  (** Your app is making too many requests and is being rate limited.
      [Too_many_requests] can trigger on a per-app or per-user
      basis. *)
  | Try_later of int option * error_description
  (** [Try_later(sec, e)] If [sec = Some s], this means your app is
      being rate limited and you must retry after [s] seconds.
      Otherwise, this indicates a transient server error, and your app
      should retry its request. *)
  | Quota_exceeded of error_description
  (** User is over Dropbox storage quota. *)
  | Server_error of int * error_description
  (** Server error 5xx *)
  | Not_modified of error_description
  (** The folder contents have not changed (relies on hash parameter). *)
  | Unsupported_media_type of error_description
  (** The image is invalid and cannot be converted to a thumbnail. *)

val string_of_error : error -> string

exception Error of error

(** Date representation. *)
module Date : sig
  type t = Dropbox_date.t

  (** Day of week. *)
  type wday = Dropbox_date.wday
            = Sun | Mon | Tue | Wed | Thu | Fri | Sat

  (** Month. *)
  type month
    = Dropbox_date.month
    = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec

  val day : t -> int      (** Day of the month (1-31). *)

  val month : t -> month  (** Month *)

  val year : t -> int     (** 4 digits year *)

  val hour : t -> int     (** Hour *)

  val min : t -> int      (** Minutes *)

  val sec : t -> int      (** Seconds *)

  val wday : t -> wday    (** Day of week *)

  val to_string : t -> string
end

(** Dropbox API. *)
module type S = sig

  (** {{:http://oauth.net/}OAuth 2.0} authentication. *)
  module OAuth2 : sig

    val authorize : ?state: string ->
                    ?force_reapprove: bool ->
                    ?disable_signup: bool ->
                    id: string ->
                    [`Token of Uri.t | `Code of Uri.t option] -> Uri.t
    (** [authorize client_id response] starts the OAuth 2.0 authorization flow.
        This isn't an API call—it's the web page that lets the user sign
        in to Dropbox and authorize your app.  The [client_id] is the
        app's key, found in the
        {{:https://www.dropbox.com/developers/apps}App Console}.  After
        the user authorizes your app, they will be sent to your redirect
        URI.  The type of response varies based on the [response]:

        - [`Token redirect_uri] (also called "implicit grant") returns
          the bearer token by redirecting the user to [redirect_uri]
          after the authorization has completed.  Extract the token
          using {!token_of_uri}.  This is useful for pure client-side
          apps, such as mobile apps or JavaScript-based apps.

        - [`Code u] if [u = Some redirect_uri], returns a code via by
          redirecting the user to [redirect_uri] (extract the code
          using {!code_of_uri}) or, if [u = None], presents the code
          to use user (on screen) who will be invited to copy it in
          your app.  The code should then be converted into a bearer
          token using {!OAuth2.token}.  This is recommended for apps
          that are running on a server.

        Note that the URI for [`Token] and [`Code] must be registered
        in the {{:https://www.dropbox.com/developers/apps}App
        Console}; even 'localhost' must be listed if it is used for
        testing.

        @param state Up to 200 bytes of arbitrary data that will be
        passed back to your redirect URI. This parameter should be used
        to protect against cross-site request forgery (CSRF).  See
        Sections
        {{:http://tools.ietf.org/html/rfc6819#section-4.4.1.8}4.4.1.8}
        and
        {{:http://tools.ietf.org/html/rfc6819#section-4.4.2.5}4.4.2.5}
        of the OAuth 2.0 threat model spec.

        @param force_reapprove Whether or not to force the user to
        approve the app again if they've already done so. If [false]
        (default), a user who has already approved the application may
        be automatically redirected to the URI specified by
        [redirect_uri].  If [true], the user will not be automatically
        redirected and will have to approve the app again.

        @param disable_signup When [true] (default is [false]) users
        will not be able to sign up for a Dropbox account via the
        authorization page.  Instead, the authorization page will show a
        link to the Dropbox iOS app in the App Store.  This is only
        intended for use when necessary for compliance with App Store
        policies.  *)

    type code = string
    (** The authorization code, which can be used to attain
        a bearer token by calling {!token}.  *)

    val code_of_uri : Uri.t -> (code * string) option
    (** [code_of_uri u] return the code and state from the redirect
        URI [u] after a [`Code] authorization. *)

    type token = string

    val token_of_uri : Uri.t -> (token * string) option
    (** [token_of_uri u] parse the URI coming from a [`Token] flow and
        extract the token and state. *)

    val token : ?redirect_uri: Uri.t ->
                code -> id: string -> secret: string -> token Lwt.t
    (** [token code id secret] acquire a token once the user has
        authorized the app.  Only applies to apps using the
        authorization [`Code] flow.

        [code] is the code acquired by directing users to
        [OAuth2.authorize ~response_type:`Code].

        [id] this should be the app's key (found in the
        {{:https://www.dropbox.com/developers/apps}App Console}).

        [secret] this parameter should be present and should be the
        app's secret.

        @param redirect_uri Only used to validate that it matches the
        original {!authorize}, not used to redirect again.  *)
  end

  type t
  (** Represent a session communicating with Dropbox. *)

  val session : OAuth2.token -> t

  val token : t -> OAuth2.token
  (** The token of the current session. *)

  type name_details
    = Dropbox_t.name_details
    = { familiar_name: string; (** The locale-dependent familiar name
                                   for the user. *)
        given_name: string; (** The user's given name. *)
        surname: string;    (** The user's surname. *)
      }

  type team = Dropbox_t.team
            = { name: string; (** The name of the team the user belongs to. *)
                team_id: int; (** The ID of the team the user belongs to. *)
              }

  type quota_info
    = Dropbox_t.quota_info
    = { shared: int; (** The user's used quota outside of shared
                         folders (bytes). *)
        quota: int;  (** The user's used quota in shared folders (bytes). *)
        normal: int; (** The user's total quota allocation (bytes). *)
      }

  type info
    = Dropbox_t.info
    = { uid: int; (** The user's unique Dropbox ID. *)
        display_name: string; (** The user's display name. *)
        email_verified: bool;
        name_details: name_details;
        referral_link: Uri.t;
        (** The user's {{:https://www.dropbox.com/referrals}referral link}. *)
        country: string;
        (** The user's two-letter country code, if available. *)
        locale: string; (** Locale preference set by the user (e.g. en-us). *)
        is_paired: bool;
        (** If true, there is a paired account associated with this user. *)
        team: team option;
        (** If the user belongs to a team, contains team information. *)
        quota_info: quota_info;
      }

  val info : ?locale: string -> t -> info Lwt.t
  (** [info t] return the information about the user's account.

      @param locale Specify language settings for user error messages
      and other language specific text.  See
      {{:https://www.dropbox.com/developers/core/docs#param.locale}Dropbox
      documentation} for more information about supported locales.  *)

  type photo_info
    = Dropbox_json.Photo.info
    = { time_taken: Date.t option;  (** The creation time of the photo *)
        lat_long: (float * float) option;
        (** The GPS coordinates of the photo, if any. *)
      }

  type video_info
    = Dropbox_json.Video.info
    = { time_taken: Date.t option;  (** The creation time of the video *)
        duration: float option;     (** The video length in ms *)
        lat_long: (float * float) option;
        (** The GPS coordinates of the video, if any. *)
      }

  type user
    = Dropbox_t.user
    = { uid: int; (** The user's unique Dropbox ID *)
        display_name: string; (** The name of the user *)
        same_team: bool;
        (** Whether the user is on the same team as the linked account *)
        member_id: string
        (** If this endpoint is called by a Dropbox for Business app and
            the user is on that team, a member_id field will also be present *)
      }

  type user_info
    = Dropbox_t.user_info
    = { user: user;
        access_type: string;
        active: bool
        (** Whether the user is active in a shared folder *)
      }

  type group = Dropbox_t.group
             = { group_name: string;
                 group_id: string;
                 num_members: int }

  type shared_folder
    = Dropbox_t.shared_folder
    = { shared_folder_id: string;
        shared_folder_name: string;
        path: string;
        access_type: string;
        shared_link_policy: string;
        owner: user option;
        membership: user_info list;
        (** The membership field only contains users who have joined the
            shared folder and does not include users who have been invited
            but have not accepted. When the active field is [false], it means
            that a user has left a shared folder (but may still rejoin). *)
        groups: group list
      }

  type metadata = Dropbox_t.metadata = {
      size: string;
      (** A human-readable description of the file size (translated by
          locale). *)
      bytes: int;      (** The file size in bytes. *)
      mime_type: string;
      path: string;    (** The canonical path to the file or folder. *)
      is_dir: bool;    (** Whether the given entry is a folder or not. *)
      is_deleted: bool; (** Whether the given entry is deleted.  (Only
                            interesting if deleted files are returned.)  *)
      rev: string;
      (** A unique identifier for the current revision of a file.  This
          field is the same [rev] as elsewhere in the API and can be
          used to detect changes and avoid conflicts. *)
      hash: string;
      (** A folder's hash is useful for indicating changes to the
          folder's contents in later calls to {!metadata}.  This is
          roughly the folder equivalent to a file's [rev].  (Is [""]
          for a file.) *)
      thumb_exists: bool;
      (** True if the file is an image that can be converted to a
          thumbnail via the {!thumbnails} call. *)
      photo_info: [ `None | `Pending | `Some of photo_info ];
      (** Only returned when the include_media_info parameter is true and the
          file is an image. A dictionary that includes the creation time
          (time_taken) and the GPS coordinates (lat_long). *)
      video_info: [ `None | `Pending | `Some of video_info ];
      (** Only returned when the include_media_info parameter is true and the
          file is a video. A dictionary that includes the creation time
          (time_taken), the GPS coordinates (lat_long), and the length of the
          video in milliseconds (duration). *)
      icon: string;
      (** The name of the icon used to illustrate the file type in Dropbox's
          {{:https://www.dropbox.com/static/images/dropbox-api-icons.zip}icon
          library}. *)
      modified: Date.t option;
      (** The last time the file was modified on Dropbox (not included
          for the root folder).  *)
      client_mtime: Date.t option;
      (** For files, this is the modification time set by the desktop
          client when the file was added to Dropbox.  Since this time
          is not verified (the Dropbox server stores whatever the
          desktop client sends up), this should only be used for
          display purposes (such as sorting) and not, for example, to
          determine if a file has changed or not. *)
      root: [ `Dropbox | `App_folder ];
      (** The root or top-level folder depending on your access
          level. All paths returned are relative to this root level. *)
      contents: metadata list;
      (** For folders, contents is the list of the metadata of the files
          contained in this folder. Return nothing if the folder is empty. *)
      shared_folder: shared_folder option;
      (** This field will be included for shared folders.
          See [shared_folder] for a sample shared folder response. *)
      read_only: bool;
      (** For shared folders, this field specifies whether the user has
          read-only access to the folder. For files within a shared folder,
          this specifies the read-only status of the parent shared folder. *)
      parent_shared_folder_id: int;
      (** For files within a shared folder, this field specifies the ID of
          the containing shared folder. *)
      modifier: user option
      (** For files within a shared folder, this field specifies which user
          last modified this file. If the modifying user no longer exists,
          the value will be null.  *)
    }

  type cursor

  type delta = {
      entries: (string * metadata option) list;
      (** A list of "delta entries".  Each delta entry is a 2-item
          list of one of the following forms:

          [(path, Some metadata)] - Indicates that there is a file/folder
          at the given path. You should add the entry to your local
          state. The metadata value is the same as what would be
          returned by the {!metadata} call, except folder metadata
          doesn't have hash or contents fields.  To correctly process
          delta entries:

          - If the new entry includes parent folders that don't yet exist in
            your local state, create those parent folders in your local state.

          - If the new entry is a file, replace whatever your local state has
            at path with the new entry.

          - If the new entry is a folder, check what your local state has at
            <path>. If it's a file, replace it with the new entry. If it's a
            folder, apply the new <metadata> to the folder, but don't modify
            the folder's children. If your local state doesn't yet include
            this path, create it as a folder.

          - If the new entry is a folder with the read-only field set to true,
            apply the read-only permission recursively to all files within the
            shared folder.

          [(path, None)] - Indicates that there is no file/folder at
          the given path. To update your local state to match,
          anything at path and all its children should be
          deleted.  Deleting a folder in your Dropbox will sometimes
          send down a single deleted entry for that folder, and
          sometimes separate entries for the folder and all child
          paths.  If your local state doesn't have anything at path,
          ignore this entry.

          Note: Dropbox treats file names in a case-insensitive but
          case-preserving way. To facilitate this, the [path] values
          above are lower-cased versions of the actual path. The last
          path component of the [metadata] value will be
          case-preserved. *)
      reset: bool;
      (** If [true], clear your local state before processing the
          delta entries.  reset is always true on the initial call to
          {!delta} (i.e., when no cursor is passed in).  Otherwise, it is
          true in rare situations, such as after server or account
          maintenance, or if a user deletes their app folder. *)
      cursor: cursor;
      (** Encodes the latest information that has been returned.  On
          the next call to {!delta}, pass in this value. *)
      has_more: bool;
      (** If [true], then there are more entries available; you can
          call {!delta} again immediately to retrieve those entries.  If
          [false], then wait for at least five minutes (preferably
          longer) before checking again. *)
      }

  type longpoll_delta
    = Dropbox_t.longpoll_delta
    = { changes: bool; (** Incidate whether new changes are available. *)
        backoff: int option;
        (** If present, it indicates how many seconds your code should
            wait before calling {!longpoll_delta} again. *)
      }

  type copy_ref
    = Dropbox_t.copy_ref
    = { copy_ref: string; (** A reference string to the specified file *)
        expires: Date.t
        (** The link's expiration date in Dropbox's usual date format. All
            links are currently set to expire far enough in the future so
            that expiration is effectively not an issue. *)
      }

  val get_file : t -> ?rev: string -> ?start: int -> ?len: int ->
                 string -> (metadata * string Lwt_stream.t) option Lwt.t
  (** [get_file t name] return the metadata for the file and a stream of
      its content.  [None] indicates that the file does not exists.

      @param start The first byte of the file to download.  A negative
      number is interpreted as [0].  Default: [0].

      @param len The number of bytes to download.  If [start] is not set,
      the last [len] bytes of the file are downloaded.  Default: download
      the entire file (or everything after the position [start],
      including [start]).  If [start <= 0], the metadata will be present
      but the stream will be empty. *)

  val metadata : t -> ?file_limit: int -> ?hash: string -> ?list: bool ->
                 ?include_deleted: bool -> ?rev: string -> ?locale: string ->
                 ?include_media_info: bool -> ?include_membership: bool ->
                 string -> metadata option Lwt.t
  (** [metadata t path] return the metadata for the file or the folder
      [path].  A return value of [None] means that the file does
      not exists.

      @param file_limit Default is 10,000 (max is 25,000). When listing a
      folder, the service won't report listings containing more than the
      specified amount of files and will instead respond with a
      [Not_Acceptable] error.

      @param hash Each call to {!metadata} on a folder will return a
      hash field, generated by hashing all of the metadata contained
      in that response.  On later calls to {!metadata}, you should
      provide that value via this parameter so that if nothing has
      changed, the response will be [Not Modified] status code
      instead of the full, potentially very large, folder listing.
      This parameter is ignored if the specified path is associated
      with a file or if [list=false].

      @param list If [true], the folder's metadata will include a
      contents field with a list of metadata entries for the contents
      of the folder.  If [false], the contents field will be empty.
      Default: [true].

      @param include_deleted Only applicable when list is set.  If
      this parameter is set to [true], then contents will include the
      metadata of deleted children.  Note that the target of the
      metadata call is always returned even when it has been deleted
      (with [is_deleted] set to [true]) regardless of this flag.

      @param rev If you include a particular revision number, then only the
      metadata for that revision will be returned.

      @param locale The metadata returned will have its size field translated
      based on the given locale. For more information see the
      {{:https://www.dropbox.com/developers/core/docs#param.locale}Dropbox
      documentation}.

      @param include_media_info If [true], each file will include a
      {!photo_info} record for photos and a {!video_info} record for
      videos with additional media info. If the data isn't available
      yet, the string pending will be returned instead of a
      dictionary.

      @param include_membership If [true], metadata for a shared folder will
      include a list of members and a list of groups.

      Possible errors:
      Not_modified The folder contents have not changed (relies on hash
      parameter).
      Not_acceptable There are too many file entries to return. *)

  val delta : ?cursor: cursor -> ?locale: string -> ?path_prefix: string ->
              ?include_media_info: bool -> t -> delta Lwt.t
  (** [delta t] return the delta.  This is a way of letting you keep
      up with changes to files and folders in a user's Dropbox.
      Deltas are instructions on how to update your local state to
      match the server's state.

      @param cursor A value that is used to keep track of your current state.
      On the next call pass in this value to return delta entries that have
      been recorded since the cursor was returned.

      @param locale Specify language settings for user error messages
      and other language specific text.  See the
      {{:https://www.dropbox.com/developers/core/docs#param.locale}Dropbox
      documentation} for more information about supported locales.

      @param path_prefix If present, this parameter filters the
      response to only include entries at or under the specified
      path. For example, a [path_prefix] of ["/Photos/Vacation"] will
      return entries for the path "/Photos/Vacation" and any files and
      folders under that path.  If [cursor] is set, [path_prefix] is
      interpreted as the sub-path of the [path_prefix] used to create
      the cursor.  For example if your cursor has no [path_prefix],
      you can switch to any [path_prefix].  If your cursor has a
      [path_prefix] of ["/Photos"], then setting [path_prefix =
      "/Vacaction"] will switch the path for this request to
      "/Photos/Vacaction".

      @param include_media_info If true (default is [false]), each
      file will include a [photo_info] record for photos and a
      [video_info] record for videos with additional media info.  When
      [include_media_info] is specified, files will only appear in
      delta responses when the media info is ready.  This parameter is
      ignored when you use a [cursor] (the Dropbox API mandates that
      the value set at the creation of the cursor is used). *)

  val latest_cursor : ?path_prefix: string -> ?include_media_info: bool ->
                      t -> cursor Lwt.t
  (** [latest_cursor t] return a cursor (as would be returned by
      {!delta} when [has_more] is [false]).

      @param path_prefix If present, the returned cursor will be
      encoded with a [path_prefix] for the specified path for use with
      {!delta}.

      @param include_media_info If [true], the returned cursor will be
      encoded with [include_media_info] set to [true] for use with
      {!delta}. *)

  val longpoll_delta : t -> ?timeout: int -> cursor -> longpoll_delta Lwt.t
  (** [longpoll_delta t cursor] blocks the connection until changes
      are available or a timeout occurs.  In both case, a value [r]
      will be returned with [r.changes] indicating whether new changes
      are available.  If this is the case, you should call {!delta} to
      retrieve the changes. If this value is [false], it means the
      call to {!longpoll_delta} timed out.  In conjunction with
      {!delta}, this call gives you a low-latency way to monitor an
      account for file changes.

      The [cursor] is a crusor returned from a call to {!delta}.  Note
      that a cursor returned from a call to {!delta} with
      [include_media_info=true] is incompatible with {!longpoll_delta}
      and an error will be returned.

      @param timeout An integer indicating a timeout, in seconds. The
      default value is 30 seconds, which is also the minimum allowed
      value.  The maximum is 480 seconds.  The request will block for
      at most this length of time, plus up to 90 seconds of random
      jitter added to avoid the thundering herd problem.  Care should
      be taken when using this parameter, as some network
      infrastructure does not support long timeouts. *)


  val revisions : t -> ?rev_limit: int -> ?locale: string -> string ->
                  metadata list option Lwt.t
  (** [revisions t name] Return the metadata for the previous revisions of
      a file (in a list of metadata).  Only revisions up to thirty days old
      are available.  A return value of [None] means that the file does
      not exist.

      @param rev_limit Default is 10. Max is 1,000. Up to this number of
      recent revisions will be returned.

      @param locale Specify language settings for user error messages
      and other language specific text. See
      {{:https://www.dropbox.com/developers/core/docs#param.locale}Dropbox
      documentation} for more information about supported locales. *)

  val restore : t -> ?locale: string -> rev:string -> string ->
                metadata option Lwt.t
  (** [restore t rev name] Restores the file path [name] to the
      previous revision [rev].  Return the metadata of the restored
      file.  A return value of [None] means that there is no file path
      [name] with such [rev].

      If the revision is not well formed or non-existing, the function
      will fail throwing [Error Dropbox.Invalid_arg].

      @param locale Specify language settings for user error messages
      and other language specific text. See
      {{:https://www.dropbox.com/developers/core/docs#param.locale}Dropbox
      documentation} for more information about supported locales. *)


  val search : t -> ?file_limit: int -> ?include_deleted: bool ->
               ?locale: string -> ?include_membership: bool ->
               ?fn: string -> string -> metadata list Lwt.t
  (** [search query] return the list containing the metadata for all files and
      folders whose filename contains the given search string as a substring.

      @param path The path to the folder you want to search from. Must be
      written entirely.

      @param query The search string. This string is split (on spaces) into
      individual words. Files and folders will be returned if they contain
      all words in the search string.

      @param file_limit The maximum and default value is 1,000. No more than
      [file_limit] search results will be returned.

      @param include_deleted If this parameter is set to [true], then
      files and folders that have been deleted will also be included
      in the search.  Default: [false].

      @param locale Specify language settings for user error messages
      and other language specific text. See
      {{:https://www.dropbox.com/developers/core/docs#param.locale}Dropbox
      documentation} for more information about supported locales.

      @param include_membership If [true], metadata for a shared
      folder will include a list of members and a list of groups.
      Default: [false]. *)

  val copy_ref : t -> string -> copy_ref option Lwt.t
  (** [copy_ref t fname] creates and return a [copy_ref] to the file
      or directory [fname].  A return value of [None] means that
      [fname] does not exist.  {!copy_ref} can be used to copy that
      file to another user's Dropbox by passing it in as the
      [from_copy_ref] parameter on {!copy}.  All links are
      currently set to expire far enough in the future so that
      expiration is effectively not an issue. *)

  type visibility = [
    | `Public
    | `Team_only
    | `Password
    | `Team_and_password
    | `Shared_folder_only
    | `Other of string
    ]
  (** The visibility of a [shared_link]. *)

  type shared_link
    = Dropbox_t.shared_link
    = { url: string; (** The link URL to the file or directory *)
        expires: Date.t; (** The link's expiration date *)
        visibility: visibility;
        (** Dropbox for Business users can set restrictions on shared
            links; the visibility field indicates what (if any)
            restrictions are set on this particular link. Possible
            values include: [`Public] (anyone can view), [`Team_only]
            (only the owner's team can view), [`Password] (a password
            is required), [`Team_and_password] (a combination of
            [`Team_only] and [`Password] restrictions), or
            [`Shared_folder_only] (only members of the enclosing
            shared folder can view).  Note that Dropbox says that
            other values may be added at any time (these will be
            captured by [`Other]). *)
      }

  val shares : t -> ?locale: string -> ?short_url: bool -> string ->
               shared_link option Lwt.t
  (** [shares t path] return a [shared_link] to a file or folder.
      A return value of [None] means that the file does not exist.

      @param locale Specify language settings for user error messages
      and other language specific text. See the
      {{:https://www.dropbox.com/developers/core/docs#param.locale}Dropbox
      documentation} for more information about supported locales.

      @param short_url When [true] (default), the URL returned will be
      shortened using the Dropbox URL shortener. If [false], the URL will link
      directly to the file's preview page. *)

  type link
    = Dropbox_t.link
    = { url: string; (** The link URL to the file *)
        expires: Date.t (** The link's expiration date *)
      }

  val media : t -> ?locale: string -> string -> link option Lwt.t
  (** [media t path] return a [link] directly to a file. A return value of
      [None] means that the file does not exist.

      Similar to {!shares}. The difference is that this bypasses the Dropbox
      webserver, used to provide a preview of the file, so that you can
      effectively stream the contents of your media. This URL should not be
      used to display content directly in the browser.

      Note that the {!media} link expires after four hours, allotting
      enough time to stream files, but not enough to leave a
      connection open indefinitely.

      @param locale Specify language settings for user error messages
      and other language specific text. See
      {{:https://www.dropbox.com/developers/core/docs#param.locale}Dropbox
      documentation} for more information about supported locales. *)

  val shared_folders :
    ?shared_folder_id: string -> ?include_membership: bool ->
    t -> shared_folder list Lwt.t
  (** [shared_folder t] Return the metadata about a specific shared folder
      or the list of all shared folders the authenticated user has access
      to if [shared_folder_id] is not specified.

      @param shared_folder The ID of a specific shared folder.

      @param include_membership If [true] (the default), include a
      list of members and a list of groups for the shared folder. *)

  val files_put :
    t -> ?locale: string -> ?overwrite: bool ->
    ?parent_rev: string -> ?autorename: bool ->
    string ->
    [ `String of string
    | `Strings of string list
    | `Stream of string Lwt_stream.t
    | `Stream_len of string Lwt_stream.t * int] -> metadata Lwt.t
  (** [files_put t path content] upload the [content] under the [path]
      (the full path is created by Dropbox if necessary) and return
      the metadata of the uploaded file.  The [path] should not point
      to a folder.

      @param locale The metadata returned on successful upload will have
      its size field translated based on the given locale.

      @param overwrite This value determines whether an existing file
      will be overwritten by this upload.  If [true] (the default),
      any existing file will be overwritten. If [false], the other
      parameters determine whether a conflict occurs and how that
      conflict is resolved.

      @param parent_rev If present, this parameter specifies the
      revision of the file you're editing. If parent_rev matches the
      latest version of the file on the user's Dropbox, that file will
      be replaced. Otherwise, a [Conflict] will occur. If you specify
      a parent_rev and that revision doesn't exist, the file won't
      save (this function will fail with [Invalid_arg]).  You can get the
      most recent revision by performing a call to {!metadata}.

      @param autorename This value determines what happens when there
      is a conflict.  If [true] (the default), the file being uploaded
      will be automatically renamed to avoid the conflict.  (For
      example, test.txt might be automatically renamed to
      test (1).txt.)  The new name can be obtained from the returned
      metadata.  If [false], the call will fail with a [Conflict]
      error.

      Possible errors:
      Fail with [Conflict] if a conflict occurred.  This means a file
      already existed at the specified path, [overwrite] was [false],
      and the [parent_rev] (if specified) didn't match the current rev.

      Fail with [Invalid_arg] Returned if the request does not contain
      an [upload_id] or if there is no chunked upload matching the
      given [upload_id]. *)

  type chunked_upload_id = private string

  type chunked_upload
    = { id: chunked_upload_id; (** The ID of the in-progress upload. *)
        ofs: int;          (** The byte offset for the next chunk. *)
        expires: Date.t    (** The time limit to finish the upload. *)
      }

  val chunked_upload :
    t -> ?id: chunked_upload_id -> ?ofs: int ->
    [ `String of string
    | `Strings of string list
    | `Stream of string Lwt_stream.t ] -> chunked_upload Lwt.t
  (** [chunked_upload chunk] upload the [chunk] and return the ID and
      offset for the subsequent upload of the same file.  This allows
      to upload large files to Dropbox (larger than 150Mb which is the
      limit for {!files_put}).  Chunks can be any size up to 150 MB.
      A typical chunk is 4 MB.  Using large chunks will mean fewer
      calls to {!chunked_upload} and faster overall throughput.
      However, whenever a transfer is interrupted, you will have to
      resume at the beginning of the last chunk, so it is often safer
      to use smaller chunks.

      @param id The unique ID of the in-progress upload on the server.
      If not set, the server will create a new upload session.

      @param ofs The byte offset of this chunk, relative to the
      beginning of the full file. The server will verify that this
      matches the offset it expects.  If it does not,
      {!chunked_upload} will fail with an [Invalid_arg] error. *)

  val commit_chunked_upload : t -> ?locale: string -> ?overwrite: bool ->
                              ?parent_rev: string -> ?autorename: bool ->
                              chunked_upload_id -> string -> metadata Lwt.t
  (** [commit_chunked_upload upload_id path] complete the upload
      initiated by {!chunked_upload}.  Save the uploaded data under
      [path] and return the metadata for the uploaded file using
      chunked_upload.  [upload_id] is used to identify the chunked
      upload session you'd like to commit.

      @param locale The metadata returned on successful upload will have its
      size field translated based on the given locale.

      @param overwrite This value, either [true] (default) or [false],
      determines whether an existing file will be overwritten by this upload.
      If [true], any existing file will be overwritten. If [false], the other
      parameters determine whether a conflict occurs and how that conflict is
      resolved.

      @param parent_rev If present, this parameter specifies the [revision] of
      the file you're editing. If parent_rev matches the latest version of the
      file on the user's Dropbox, that file will be replaced. Otherwise, a
      conflict will occur. (See below.) If you specify a parent_rev and that
      [revision] doesn't exist, the file won't save. You can get the most
      recent rev by performing a call to [metadata].

      @param autorename This value, either [true] (default) or [false],
      determines what happens when there is a conflict. If [true], the file
      being uploaded will be automatically renamed to avoid the conflict.
      (For example, test.txt might be automatically renamed to test (1).txt.)
      The new name can be obtained from the returned metadata. If [false],
      the call will fail with a Conflict response code.

      Possible errors:
      Fail with [Conflict] if a conflict occurred. This means a file
      already existed at the specified path, [overwrite] was [false],
      and the [parent_rev] (if specified) didn't match the current rev.

      Fail with [Invalid_arg] if there is no chunked upload matching
      the given [upload_id]. *)

  val thumbnails : t -> ?format: [ `Jpeg | `Png | `Bmp ]
                   -> ?size: [ `Xs | `S | `M | `L | `Xl ] -> string ->
                   (metadata * string Lwt_stream.t) option Lwt.t
  (** [thumbnails t path] return the metadata for the thumbnails of
      [path] and a stream of the content of the thumbnails.  [None]
      indicates that the [path] does not exists or is not an image.

      Note that This method currently supports files with the following file
      extensions: .jpg, .jpeg, .png, .tiff, .tif, .gif, .bmp (it also work on
      .avi, .mp4, .flv).  And photos larger than 20MB in size won't be
      converted to a thumbnail.

      @param format [`Jpeg] (default) or [`Png]. For images that are photos,
      [`Jpeg] should be preferred, while [`Png] is better for screenshots
      and digital art. Support also [`Bmp].

      @param size The size of the thumbnail (default: [`S]): [`Xs]
      (32x32), [`S] (64x64), [`M] (128x128), [`L] (640x480), [`Xl]
      (1024x768).  The image returned may be larger or smaller than
      the size requested, depending on the size and aspect ratio of
      the original image. *)

  val previews : t -> ?rev: string -> string ->
                 (string * string * string Lwt_stream.t) option Lwt.t
  (** [previews t path] Returns a 3-uple which contains the
      Content-Type whose values are ["application/pdf"] or
      ["text/html"], the Original-Content-Length which is the size of
      the preview data and the stream of its content.  Return [None]
      when the file wasn't found the specified path, or wasn't found
      at the specified [rev].

      Note previews are only generated for the files with the following
      extensions: .doc, .docx, .docm, .ppt, .pps, .ppsx, .ppsm, .pptx,
      .pptm, .xls, .xlsx, .xlsm, .rtf.

      @param rev The revision of the file to retrieve. This defaults to
      the most recent revision. *)


  (** {2:fileops File Operations} *)

  type root = [ `Auto | `Dropbox | `Sandbox ]

  val copy : t -> ?locale: string -> ?root: root ->
             [ `From_path of string | `From_copy_ref of string ] ->
             string -> [ `Some of metadata
                       | `None | `Invalid of string
                       | `Too_many_files ] Lwt.t
  (** [copy t source to_path] copy the file or folder.  The [source]
      can either be:
      - [`From_path]: specifies the file or folder to be copied from
                      relative to the root;
      - [`From_copy_ref]: specifies a [copy_ref] generated from a previous
                          {!copy_ref} call.

      The second argument [to_path] specifies the destination path,
      including the new name for the file or folder, relative to [root].
      If everything goes well, the metadata of the copy is returned.
      - [`None] means that the [source] was not found.
      - [`Invalid] is returned when there is already a file at the
        given destination, or one is trying to copy a shared folder
      - [`Too_many_files] is returned when too many files would be
        involved in the operation for it to complete successfully.  The
        limit is currently 10,000 files and folders.

      @param root The root relative to which [from_path] and [to_path] are
      specified. Valid values are `Auto (default), `Sandbox, and `Dropbox.

      @param locale Specify language settings for user error messages
      and other language specific text.  See
      {{:https://www.dropbox.com/developers/core/docs#param.locale}Dropbox
      documentation} for more information about supported locales.  *)

  val create_folder : t -> ?locale: string -> ?root: root ->
                      string -> [ `Some of metadata
                                | `Invalid of string ] Lwt.t
  (** [create_folder path] create the folder [path] (interpreted
      relatively to [root]) and return the metadata for the new folder.
      Return [`Invalid] if [path] already exists.

      @param root The root relative to which path is specified. Valid values
      are [`Auto] (default), [`Sandbox], and [`Dropbox].

      @param locale Specify language settings for user error messages
      and other language specific text.  See
      {{:https://www.dropbox.com/developers/core/docs#param.locale}Dropbox
      documentation} for more information about supported locales. *)

  val delete :
    t -> ?locale: string -> ?root: root ->
    string -> [ `Some of metadata | `None | `Too_many_files ] Lwt.t
  (** [delete path] delete the file or folder [path] and return the
      metadata of it.  Return [`None] if [path] does not exist and
      [`Too_many_files] when too many files would be involved in the
      operation for it to complete successfully.  The limit is
      currently 10,000 files and folders.

      @param root The root relative to which path is specified. Valid values
      are [`Auto] (default), [`Sandbox], and [`Dropbox].

      @param path The path to the file or folder to be deleted.

      @param locale Specify language settings for user error messages
      and other language specific text.  See
      {{:https://www.dropbox.com/developers/core/docs#param.locale}Dropbox
      documentation} for more information about supported locales. *)

  val move : t -> ?locale: string -> ?root: root ->
             string -> string -> [ `Some of metadata
                                 | `None
                                 | `Invalid of string
                                 | `Too_many_files ] Lwt.t
  (** [move from_path to_path] move the file or folder [from_path]
      to [to_path].  If everything goes well, the metadata of the
      moved file is returned.
      - [`None] means that the [from_path] does not exist.
      - [`Invalid] is returned when there is already a file at the
        given destination, or one tries to move a shared folder
        into a shared folder.
      - [`Too_many_files] is returned when too many files would be
        involved in the operation for it to complete successfully.  The
        limit is currently 10,000 files and folders.

      @param root The root relative to which from_path and to_path are
      specified.

      @param locale Specify language settings for user error messages
      and other language specific text.  See
      {{:https://www.dropbox.com/developers/core/docs#param.locale}Dropbox
      documentation} for more information about supported locales. *)
end

module Make(Client: Cohttp_lwt.S.Client) : S
(** Create a concrete Dropbox API implementation given a client one.
    Note that several instances have been instantiated for you in the
    Dropbox_* modules so you generally do not have to call this
    yourself. *)
;;
