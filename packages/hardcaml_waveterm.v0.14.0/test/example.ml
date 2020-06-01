open! Import

module I = struct
  type 'a t =
    { clk : 'a
    ; clr : 'a
    ; a : 'a [@bits 16]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { b : 'a [@bits 16] } [@@deriving sexp_of, hardcaml]
end

let create (i : Signal.t I.t) =
  let open Signal in
  let reg_spec = Reg_spec.create () ~clock:i.clk ~clear:i.clr in
  { O.b = reg reg_spec ~enable:vdd (i.a +:. 1) }
;;

let testbench () =
  let module Sim = Cyclesim.With_interface (I) (O) in
  let sim = Sim.create ~is_internal_port:Signal.has_name create in
  let i = Cyclesim.inputs sim in
  let waveform, sim = Waveform.create sim in
  i.clr := Bits.vdd;
  Cyclesim.cycle sim;
  i.clr := Bits.gnd;
  i.a := Bits.of_int ~width:16 23;
  Cyclesim.cycle sim;
  i.a := Bits.of_int ~width:16 45;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  waveform
;;
