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
    | O_KEEPEXEC [@if ocaml_version >= (4, 05, 0)]
  [@@deriving sexp_of]
end

module At_flag = struct
  type t =
    | AT_EACCESS
    | AT_SYMLINK_FOLLOW
    | AT_SYMLINK_NOFOLLOW
    | AT_REMOVEDIR
  [@@deriving sexp_of]
end

module Access_permission = struct
  type t = Unix.access_permission =
    | R_OK
    | W_OK
    | X_OK
    | F_OK
  [@@deriving sexp_of]
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
  [@@deriving sexp_of]
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
  [@@deriving sexp_of]
end
