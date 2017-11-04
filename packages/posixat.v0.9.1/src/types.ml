open Base

module Fd = struct
  type t = Unix.file_descr

  type info =
    | Win32_handle of int64
    | Win32_socket of int64
    | Unix_fd of int

  external info : t -> info = "shexp_fd_info"

  let sexp_of_t t =
    match info t with
    | Unix_fd n      -> sexp_of_int n
    | Win32_handle h -> List [Atom "HANDLE"; Atom (Printf.sprintf "0x%Lx" h)]
    | Win32_socket s -> List [Atom "SOCKET"; Atom (Printf.sprintf "%Lx" s)]
end

module Open_flag = struct
  type t = Unix.open_flag =
    | O_RDONLY
    | O_WRONLY
    | O_RDWR
    | O_NONBLOCK
    | O_APPEND
    | O_CREAT
    | O_TRUNC
    | O_EXCL
    | O_NOCTTY
    | O_DSYNC
    | O_SYNC
    | O_RSYNC
    | O_SHARE_DELETE
    | O_CLOEXEC
    | O_KEEPEXEC
  [@@deriving_inline sexp_of]
  let sexp_of_t : t -> Sexplib.Sexp.t =
    function
    | O_RDONLY  -> Sexplib.Sexp.Atom "O_RDONLY"
    | O_WRONLY  -> Sexplib.Sexp.Atom "O_WRONLY"
    | O_RDWR  -> Sexplib.Sexp.Atom "O_RDWR"
    | O_NONBLOCK  -> Sexplib.Sexp.Atom "O_NONBLOCK"
    | O_APPEND  -> Sexplib.Sexp.Atom "O_APPEND"
    | O_CREAT  -> Sexplib.Sexp.Atom "O_CREAT"
    | O_TRUNC  -> Sexplib.Sexp.Atom "O_TRUNC"
    | O_EXCL  -> Sexplib.Sexp.Atom "O_EXCL"
    | O_NOCTTY  -> Sexplib.Sexp.Atom "O_NOCTTY"
    | O_DSYNC  -> Sexplib.Sexp.Atom "O_DSYNC"
    | O_SYNC  -> Sexplib.Sexp.Atom "O_SYNC"
    | O_RSYNC  -> Sexplib.Sexp.Atom "O_RSYNC"
    | O_SHARE_DELETE  -> Sexplib.Sexp.Atom "O_SHARE_DELETE"
    | O_CLOEXEC  -> Sexplib.Sexp.Atom "O_CLOEXEC"
    | O_KEEPEXEC  -> Sexplib.Sexp.Atom "O_KEEPEXEC"
  [@@@end]
end

module At_flag = struct
  type t =
    | AT_EACCESS
    | AT_SYMLINK_FOLLOW
    | AT_SYMLINK_NOFOLLOW
    | AT_REMOVEDIR
  [@@deriving_inline sexp_of]
  let sexp_of_t : t -> Sexplib.Sexp.t =
    function
    | AT_EACCESS  -> Sexplib.Sexp.Atom "AT_EACCESS"
    | AT_SYMLINK_FOLLOW  -> Sexplib.Sexp.Atom "AT_SYMLINK_FOLLOW"
    | AT_SYMLINK_NOFOLLOW  -> Sexplib.Sexp.Atom "AT_SYMLINK_NOFOLLOW"
    | AT_REMOVEDIR  -> Sexplib.Sexp.Atom "AT_REMOVEDIR"
  [@@@end]
end

module Access_permission = struct
  type t = Unix.access_permission =
    | R_OK
    | W_OK
    | X_OK
    | F_OK
  [@@deriving_inline sexp_of]
  let sexp_of_t : t -> Sexplib.Sexp.t =
    function
    | R_OK  -> Sexplib.Sexp.Atom "R_OK"
    | W_OK  -> Sexplib.Sexp.Atom "W_OK"
    | X_OK  -> Sexplib.Sexp.Atom "X_OK"
    | F_OK  -> Sexplib.Sexp.Atom "F_OK"
  [@@@end]
end

module File_perm = struct
  type t = Unix.file_perm

  let sexp_of_t t = Sexp.Atom (Printf.sprintf "0o%3o" t)
end

module File_kind = struct
  type t = Unix.file_kind =
    | S_REG
    | S_DIR
    | S_CHR
    | S_BLK
    | S_LNK
    | S_FIFO
    | S_SOCK
  [@@deriving_inline sexp_of]
  let sexp_of_t : t -> Sexplib.Sexp.t =
    function
    | S_REG  -> Sexplib.Sexp.Atom "S_REG"
    | S_DIR  -> Sexplib.Sexp.Atom "S_DIR"
    | S_CHR  -> Sexplib.Sexp.Atom "S_CHR"
    | S_BLK  -> Sexplib.Sexp.Atom "S_BLK"
    | S_LNK  -> Sexplib.Sexp.Atom "S_LNK"
    | S_FIFO  -> Sexplib.Sexp.Atom "S_FIFO"
    | S_SOCK  -> Sexplib.Sexp.Atom "S_SOCK"
  [@@@end]
end

module Stats = struct
  type t = Unix.LargeFile.stats =
    { st_dev   : int
    ; st_ino   : int
    ; st_kind  : File_kind.t
    ; st_perm  : File_perm.t
    ; st_nlink : int
    ; st_uid   : int
    ; st_gid   : int
    ; st_rdev  : int
    ; st_size  : int64
    ; st_atime : float
    ; st_mtime : float
    ; st_ctime : float
    }
  [@@deriving_inline sexp_of]
  let sexp_of_t : t -> Sexplib.Sexp.t =
    function
    | { st_dev = v_st_dev; st_ino = v_st_ino; st_kind = v_st_kind;
        st_perm = v_st_perm; st_nlink = v_st_nlink; st_uid = v_st_uid;
        st_gid = v_st_gid; st_rdev = v_st_rdev; st_size = v_st_size;
        st_atime = v_st_atime; st_mtime = v_st_mtime; st_ctime = v_st_ctime }
      ->
      let bnds = []  in
      let arg = sexp_of_float v_st_ctime  in
      let bnd = Sexplib.Sexp.List [Sexplib.Sexp.Atom "st_ctime"; arg]  in
      let bnds = bnd :: bnds  in
      let arg = sexp_of_float v_st_mtime  in
      let bnd = Sexplib.Sexp.List [Sexplib.Sexp.Atom "st_mtime"; arg]  in
      let bnds = bnd :: bnds  in
      let arg = sexp_of_float v_st_atime  in
      let bnd = Sexplib.Sexp.List [Sexplib.Sexp.Atom "st_atime"; arg]  in
      let bnds = bnd :: bnds  in
      let arg = sexp_of_int64 v_st_size  in
      let bnd = Sexplib.Sexp.List [Sexplib.Sexp.Atom "st_size"; arg]  in
      let bnds = bnd :: bnds  in
      let arg = sexp_of_int v_st_rdev  in
      let bnd = Sexplib.Sexp.List [Sexplib.Sexp.Atom "st_rdev"; arg]  in
      let bnds = bnd :: bnds  in
      let arg = sexp_of_int v_st_gid  in
      let bnd = Sexplib.Sexp.List [Sexplib.Sexp.Atom "st_gid"; arg]  in
      let bnds = bnd :: bnds  in
      let arg = sexp_of_int v_st_uid  in
      let bnd = Sexplib.Sexp.List [Sexplib.Sexp.Atom "st_uid"; arg]  in
      let bnds = bnd :: bnds  in
      let arg = sexp_of_int v_st_nlink  in
      let bnd = Sexplib.Sexp.List [Sexplib.Sexp.Atom "st_nlink"; arg]  in
      let bnds = bnd :: bnds  in
      let arg = File_perm.sexp_of_t v_st_perm  in
      let bnd = Sexplib.Sexp.List [Sexplib.Sexp.Atom "st_perm"; arg]  in
      let bnds = bnd :: bnds  in
      let arg = File_kind.sexp_of_t v_st_kind  in
      let bnd = Sexplib.Sexp.List [Sexplib.Sexp.Atom "st_kind"; arg]  in
      let bnds = bnd :: bnds  in
      let arg = sexp_of_int v_st_ino  in
      let bnd = Sexplib.Sexp.List [Sexplib.Sexp.Atom "st_ino"; arg]  in
      let bnds = bnd :: bnds  in
      let arg = sexp_of_int v_st_dev  in
      let bnd = Sexplib.Sexp.List [Sexplib.Sexp.Atom "st_dev"; arg]  in
      let bnds = bnd :: bnds  in Sexplib.Sexp.List bnds

  [@@@end]
end
