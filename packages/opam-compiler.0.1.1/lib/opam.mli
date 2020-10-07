val create :
  Runner.t ->
  name:Switch_name.t ->
  description:string ->
  (unit, [ `Unknown ]) result

val pin_add :
  Runner.t ->
  name:Switch_name.t ->
  string ->
  configure_command:Bos.Cmd.t option ->
  (unit, [ `Unknown ]) result

val update : Runner.t -> name:Switch_name.t -> (unit, [ `Unknown ]) result

val reinstall_compiler :
  Runner.t -> configure_command:Bos.Cmd.t option -> (unit, [ `Unknown ]) result

val reinstall_packages : Runner.t -> (unit, [ `Unknown ]) result
