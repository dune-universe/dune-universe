(** This module launches processes isolated from the main environment
   using sandboxing technology. *)

type conf
(** Sandbox configuration.

   You can create one using the functions below.
   Example: [conf() |> mount "/usr"].  *)

val bare : conf
(** Configuration with all sharing disabled and an empty environment. *)

val conf : ?uid: int -> ?gid: int -> unit -> conf
(** Create a configuration with all sharing disabled, mounting in
   read-only mode /bin, /usr, /lib, /lib32 and /lib64 (if they exist)
   and on tmpfs /tmp, /run and /var.  The hostname is set to
   "OCaml". *)

val share_user : bool -> conf -> conf
val share_ipc  : bool -> conf -> conf
val share_pid  : bool -> conf -> conf
val share_net  : bool -> conf -> conf
val share_uts  : bool -> conf -> conf
val share_cgroup : bool -> conf -> conf

val uid : int -> conf -> conf
(** [uid c id] use a custom user [id] in the sandbox.  Automatically
   implies [share_user c false].  If [id < 0], unset it. *)

val gid : int -> conf -> conf
(** [gid c id] use a custom group [id] in the sandbox.  Automatically
   implies [share_user c false].  If [id < 0], unset it. *)

val hostname : string -> conf -> conf
(** [hostname c h] use the custom hostname [h] in the sandbox.
   Automatically implies [share_uts c false].  If [h = ""], unset it. *)

val setenv : string -> string -> conf -> conf
(** [setenv c var v] add the variable [var] with value [v] to the
   environment of the process. *)

val unsetenv : string -> conf -> conf
(** [unsetenv c var] remove the environment variable [var]. *)

(** {2 Filesystem related options} *)

val mount : ?dev:bool -> ?src:string -> ?rw:bool -> string -> conf -> conf
(** [mount c src dest] mount the host path [src] on [dest] in the
   sandbox.  The mounts are applied in the order they are set, the
   latter ones being able undo what the previous ones did.  Any
   missing parent directories that are required to create a specified
   destination are automatically created as needed.

   @param src if not provided, it defaults to [dest].
   @param dev If [true], allow device access.
   @param rw  If [true], mount in read and write (default, mount read-only).

   Example: [let c = mount c "/a" "/a" in mount c ~rw:true "/a/b" "/a/b"] *)

val remount_ro : string -> conf -> conf
(** [remount_ro c dest] remount the path [dest] as readonly.  It works
   only on the specified mount point, without changing any other mount
   point under the specified path.  *)

val proc : string -> conf -> conf
(** [proc c dest] mount procfs on [dest].
   Example: [proc c "/proc"]. *)

val dev : string -> conf -> conf
(** [dev c dest] mount new devtmpfs on [dest].
   Example: [dev c "/dev"]. *)

val tmpfs : string -> conf -> conf
(** [tmpfs c dest] mount new tmpfs on [dest].
   Example: [tmpfs c "/var"] or [tmpfs c "/tmp"]. *)

val mqueue : string -> conf -> conf
(** [mqueue c dest] mount new mqueue on [dest]. *)

val dir : string -> conf -> conf
(** [dir c dest] create directory [dest] in the sandbox. *)

(* val file : conf -> Unix.file_descr -> string -> conf
 * val bind_data : conf -> Unix.file_descr -> ?ro: bool -> string -> conf *)

val symlink : ?src:string -> string -> conf -> conf
(** [symlink c src dest] create a symlink at [dest] with target [src].
   @param src If not provided, it defaults to [dest]. *)

val chdir : string -> conf -> conf
(** [chdir dir] change directory to [dir] in the sandboxed environment. *)

val new_session : bool -> conf -> conf
(** [new_session c b] when [b] is [true], create a new terminal
   session for the sandbox (calls setsid()).  This disconnects the
   sandbox from the controlling terminal which means the sandbox can't
   for instance inject input into the terminal.

   Note: In a general sandbox, if you don't use [new_session c true],
   it is recommended to use seccomp to disallow the TIOCSTI ioctl,
   otherwise the application can feed keyboard input to the terminal.
 *)

val die_with_parent : bool -> conf -> conf
(** [die_with_parent c b]: when [b] is [true], ensures that the
   sandboxed command dies when the program using this library dies.
   Kills (SIGKILL) all sandbox processes in sequence from parent to
   child including the sandboxed command process when the process
   using this library dies.  *)


(** {2 Launch sandboxed processes} *)

val open_process_in : conf -> string -> string list -> in_channel
(** [open_process_in c cmd args] runs the command [cmd] with arguments
   [args] in a sandbox in parallel with the program.  The standard
   output of the program can be read on the returned channel. *)

val close_process_in : in_channel -> Unix.process_status


val open_process_out : conf -> string -> string list -> out_channel
(** [open_process_out c cmd args] runs the command [cmd] with
   arguments [args] in a sandbox in parallel with the program.  *)

val close_process_out : out_channel -> Unix.process_status


val open_process : conf -> string -> string list -> in_channel * out_channel
(** [open_process c cmd args] runs the command [cmd] with arguments
   [args] in a sandbox in parallel with the program.  *)

val close_process : in_channel * out_channel -> Unix.process_status


val open_process_full :
  conf -> string -> string list -> in_channel * out_channel * in_channel
(** [open_process_full c cmd args] runs the command [cmd] with
   arguments [args] in a sandbox in parallel with the program.
   The result is a triple of channels connected respectively to the
   standard output, standard input, and standard error of the
   command. *)

val close_process_full :
  in_channel * out_channel * in_channel -> Unix.process_status
