open Lwt.Syntax

let name = "ask_integrator"
let log_src = Logs.Src.create name

module Logs = (val Logs.src_log log_src : Logs.LOG)

module type Sig = sig
  type t =
    { id : string
    ; member_id : string
    ; label : string
    ; questionnaires : (string * Ask.Model.Questionnaire.t) list
    ; created_at : Ptime.t
    ; updated_at : Ptime.t
    }

  val create
    :  member_id:string
    -> member_label:string
    -> questionnaires:(string * Ask.Model.Questionnaire.t) list
    -> (Model.Handler.t, string) result Lwt.t

  val find : member_id:string -> ?label:string -> unit -> Model.Handler.t option Lwt.t

  val find_questionnaire_with_label
    :  member_id:string
    -> ?member_label:string
    -> questionnaire_label:string
    -> unit
    -> Ask.Model.Questionnaire.t option Lwt.t

  val update
    :  Model.Handler.t
    -> ?member_label:string
    -> ?questionnaires:(string * Ask.Model.Questionnaire.t) list
    -> unit
    -> (string, string) result Lwt.t

  val delete : Model.Handler.t -> unit Lwt.t

  module Internal__ : sig
    (** USE ON YOUR OWN RISK -- Internal__ functions are used for testing *)
    val clean : unit -> unit Lwt.t
  end

  val register : unit -> Sihl.Container.Service.t

  include Sihl.Container.Service.Sig
end

module Make (Repo : Repository.Sig) (AskService : Ask.Sig) = struct
  type t =
    { id : string
    ; member_id : string
    ; label : string
    ; questionnaires : (string * Ask.Model.Questionnaire.t) list
    ; created_at : Ptime.t
    ; updated_at : Ptime.t
    }

  let create ~member_id ~member_label ~questionnaires =
    let handler =
      Model.Handler.create ~member_id ~label:member_label ~questionnaires ()
    in
    let* is_unique = Repo.is_unique ~member_id ~member_label () in
    if is_unique
    then
      Lwt.catch
        (fun () ->
          let* () = Repo.insert handler in
          Lwt.return (Ok handler))
        (fun exn ->
          Logs.err (fun m ->
              m
                "An error occurred inserting ask integrator handler: %s"
                (Printexc.to_string exn));
          Lwt.return
            (Error
               "Persisting the ask integrator mappers in the database failed, please try \
                again or contact the administrator!"))
    else Lwt.return (Error "An integrator for the current handler already exists!")
  ;;

  let find ~member_id ?label () =
    let member_label = label |> Option.value ~default:"" in
    Repo.find_by_member ~member_id ~member_label
  ;;

  let find_questionnaire_with_label ~member_id ?member_label ~questionnaire_label () =
    let member_label = member_label |> Option.value ~default:"" in
    let* handler = Repo.find_by_member ~member_id ~member_label in
    match handler with
    | None -> Lwt.return_none
    | Some handler ->
      let questionnaire =
        List.find_opt
          (fun (label, _) -> String.equal label questionnaire_label)
          (Model.Handler.questionnaires handler)
      in
      (match questionnaire with
      | None -> Lwt.return_none
      | Some (_, questionnaire) -> Lwt.return_some questionnaire)
  ;;

  let update handler ?member_label ?questionnaires () =
    Repo.update handler ?member_label ?questionnaires ()
  ;;

  let delete handler = Repo.delete handler

  module Internal__ = struct
    (** USE ON YOUR OWN RISK -- Internal__ functions are used for testing *)
    let clean = Repo.clean
  end

  let start () = Lwt.return ()
  let stop _ = Lwt.return ()

  let lifecycle =
    Sihl.Container.create_lifecycle
      "ask"
      ~dependencies:(fun () -> Repo.lifecycles)
      ~start
      ~stop
  ;;

  let register () =
    Repo.register_migration ();
    Repo.register_cleaner ();
    Sihl.Container.Service.create lifecycle
  ;;
end

module MariaDb = Make (Repository.MariaDb) (Ask.MariaDb)
