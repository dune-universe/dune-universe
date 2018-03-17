type 'a configured_side_effect =
  | Return of 'a
  | Raise of exn

type 'a side_effect = 'a configured_side_effect option

let return x = Some (Return x)

let raise e = Some (Raise e)

let not_configured = None

let eval_side_effect = function
  | Return x -> x
  | Raise e -> Pervasives.raise e

type ('args, 'ret) t =
  { name: string
  ; params: 'args list ref
  ; side_effect: 'ret side_effect ref
  }

let make ~name =
  { name
  ; params = ref []
  ; side_effect = ref not_configured
  }

let name mock =
  mock.name

let configure t side_effect =
  t.side_effect := side_effect;
  t.params := []

exception Mock_not_configured of string

let call mock args =
  match !(mock.side_effect) with
  | None
    ->
    Pervasives.raise (Mock_not_configured mock.name)
  | Some effect
    ->
    begin
      mock.params := args :: !(mock.params);
      eval_side_effect effect
    end

let recorded_calls mock =
  List.rev !(mock.params)
