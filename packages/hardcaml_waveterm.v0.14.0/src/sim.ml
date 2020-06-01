open! Import
module Bits = Hardcaml.Bits
module Cyclesim = Hardcaml.Cyclesim

let wrap ?cfg sim =
  let cycle = ref 0 in
  let compare =
    match cfg with
    | None -> String.compare
    | Some s ->
      let f =
        let s = List.mapi ~f:(fun i (n, _) -> n, i) s in
        fun x -> List.Assoc.find s x ~equal:String.equal
      in
      fun a b ->
        (match f a, f b with
         | None, None -> String.compare a b
         | Some _, None -> -1
         | None, Some _ -> 1
         | Some a, Some b -> Int.compare a b)
  in
  let get_type = function
    | None -> fun _ -> Wave_format.Binary
    | Some l ->
      fun n ->
        (try List.Assoc.find_exn l n ~equal:String.equal with
         | _ -> Wave_format.Binary)
  in
  let port (n, v) =
    match n with
    | "clock" | "clk" -> Wave.Clock n, fun _ -> ()
    | "reset" | "rst" ->
      let d = Data.create () in
      Wave.Binary (n, d), fun v -> Data.set d !cycle (if v then Bits.vdd else Bits.gnd)
    | _ ->
      let t = get_type cfg n in
      let d = Data.create () in
      let wave =
        if Bits.width !v = 1 && Poly.equal t Wave_format.Binary
        then Wave.Binary (n, d)
        else Wave.Data (n, d, t, Wave_format.Left)
      in
      wave, fun _ -> Data.set d !cycle !v
  in
  let ports =
    List.concat
      [ List.map (Cyclesim.in_ports sim) ~f:port
      ; List.map (Cyclesim.out_ports ~clock_edge:Before sim) ~f:port
      ; List.map (Cyclesim.internal_ports sim) ~f:port
      ]
  in
  let ports =
    List.sort
      ~compare:(fun (w0, _) (w1, _) -> compare (Wave.get_name w0) (Wave.get_name w1))
      ports
  in
  let waves = Array.of_list (List.map ~f:fst ports) in
  let updates = Array.of_list (List.map ~f:snd ports) in
  let tasks rst () =
    Array.iter ~f:(fun f -> f rst) updates;
    Int.incr cycle
  in
  let sim =
    Cyclesim.Private.modify
      sim
      [ After, Reset, tasks true; After, At_clock_edge, tasks false ]
  in
  sim, waves
;;
