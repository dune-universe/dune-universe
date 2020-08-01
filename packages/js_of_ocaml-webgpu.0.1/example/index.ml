open Base
open Lwt
open Js_of_ocaml
open Js
module Html = Dom_html
open Webgpu
open Js_of_ocaml_webidl.Runtime

exception Shutdown

let debug_o o = Firebug.console##log o
let debug s = Firebug.console ## (log (Js.string (Sexp.to_string_hum s)))

let console_error f =
  Printf.ksprintf (fun s -> Firebug.console ## (error (Js.string s))) f
;;

(* CR-someday: do something about dev vs prod. *)
let error = console_error

let add_event_listener elt event ~f =
  (Html.addEventListener
     elt
     event
     (Html.handler (fun ev ->
          (try f ev with
          | Shutdown -> raise Shutdown
          | exn -> error "uncaught exn in handler: %s" (Exn.to_string exn));
          Js._true))
     Js._true
    : Html.event_listener_id)
  |> ignore
;;

let top_level f =
  add_event_listener Html.window Html.Event.load ~f:(fun _ ->
      Lwt.async (fun () -> Lwt.catch f raise))
;;

let array_of_list xs =
  let a = new%js Js.array_empty in
  List.iteri xs ~f:(fun i x -> Js.array_set a i x);
  a
;;

module Let_syntax = struct
  (* let return = return *)
  let bind t ~f = bind t f

  (*
     let map t ~f = map t f
     let both t1 t2 = t1 >>= fun t1 -> t2 >>= fun t2 -> return (t1, t2)
  *)
end

let matrixDimension = 512
let matrixElements = matrixDimension * matrixDimension
let matrixSize = matrixElements * 4

(* Not on the slides. Local size in X and Y. Without this the GPU will only run
   one instance of the compute shader on a block of (for example) 32 ALUs,
   wasting 31 of them.  *)
let localSize = 8

let create_buffer device ~usage : GPUBuffer.t =
  let descriptor = GPUBufferDescriptor.create ~size:matrixSize ~usage () in
  device##createBuffer ~descriptor
;;

let create_buffer_mapped device ~usage
    : GPUBuffer.t * Typed_array.arrayBuffer Js.t
  =
  let descriptor = GPUBufferDescriptor.create ~size:matrixSize ~usage () in
  let buffers = device##createBufferMapped ~descriptor in
  let gpu =
    buffers##shift
    |> Js.Optdef.to_option
    |> Option.value_exn
    |> Js.Unsafe.coerce
  in
  let cpu =
    buffers##shift
    |> Js.Optdef.to_option
    |> Option.value_exn
    |> Js.Unsafe.coerce
  in
  gpu, cpu
;;

let copy_array ~from ~to_ =
  for i = 0 to from##.length - 1 do
    let x = Typed_array.unsafe_get from i in
    Typed_array.set to_ i x
  done
;;

let random_floats () =
  let open Float in
  let a = new%js Typed_array.float32Array matrixElements in
  for i = 0 to Int.(a##.length - 1) do
    Typed_array.set a i (Random.float 1. * 10.)
  done;
  a
;;

let src =
  Printf.sprintf
    {|
      #version 450
        layout(std430, set = 0, binding = 0) readonly buffer MatrixA {
            float data[];
        } A;
        layout(std430, set = 0, binding = 1) readonly buffer MatrixB {
            float data[];
        } B;
        layout(std430, set = 0, binding = 2) buffer MatrixC {
            float data[];
        } C;
        layout(local_size_x = %d, local_size_y = %d) in;

        void main() {
            uvec2 resultCell = gl_GlobalInvocationID.xy;
            uint resultIndex = resultCell.y + resultCell.x * %d;

            float result = 0.0f;
            for (uint i = 0; i < %d; i++) {
                uint aCell = i + resultCell.x * %d;
                uint bCell = resultCell.y + i * %d;
                result += A.data[aCell] * B.data[bCell];
            }
            C.data[resultIndex] = result;
        }
  |}
    localSize
    localSize
    matrixDimension
    matrixDimension
    matrixDimension
    matrixDimension
;;

let computeOnGpu matrixA matrixB =
  debug [%message "1"];
  let%bind code = Glslang.compile (Js.string src) |> Lwt_promise.to_lwt in
  let gpu = Webgpu.get () in
  let%bind adapter =
    gpu##requestAdapter ~options:Js.undefined |> Lwt_promise.to_lwt
  in
  let%bind device =
    adapter##requestDevice ~descriptor:Js.undefined |> Lwt_promise.to_lwt
  in
  let gpuMatrixA, cpuMatrixA =
    create_buffer_mapped device ~usage:GPUBufferUsage._STORAGE
  in
  let cpuMatrixA = new%js Typed_array.float32Array_fromBuffer cpuMatrixA in
  copy_array ~from:matrixA ~to_:cpuMatrixA;
  gpuMatrixA##unmap;
  let gpuMatrixB, cpuMatrixB =
    create_buffer_mapped device ~usage:GPUBufferUsage._STORAGE
  in
  let cpuMatrixB = new%js Typed_array.float32Array_fromBuffer cpuMatrixB in
  copy_array ~from:matrixB ~to_:cpuMatrixB;
  gpuMatrixB##unmap;
  let gpuMatrixC =
    create_buffer device ~usage:GPUBufferUsage.(_STORAGE lor _COPY_SRC)
  in
  let bind_group_layout =
    let entry i =
      GPUBindGroupLayoutEntry.create
        ~binding:i
        ~visibility:GPUShaderStage._COMPUTE
        ~type_:(Js.string "storage-buffer")
        ()
    in
    let entries : GPUBindGroupLayoutEntry.t js_array Js.t =
      [ entry 0; entry 1; entry 2 ] |> array_of_list
    in
    let descriptor = GPUBindGroupLayoutDescriptor.create ~entries () in
    device##createBindGroupLayout ~descriptor
  in
  let pipeline_layout =
    let descriptor =
      GPUPipelineLayoutDescriptor.create
        ~bindGroupLayouts:(array_of_list [ bind_group_layout ])
        ()
    in
    device##createPipelineLayout ~descriptor
  in
  let bindGroup =
    let resource buffer =
      GPUBufferBinding.create ~buffer () |> Union.of_b |> Union.of_b
    in
    let entries =
      GPUBindGroupEntry.
        [ create ~binding:0 ~resource:(resource gpuMatrixA) ()
        ; create ~binding:1 ~resource:(resource gpuMatrixB) ()
        ; create ~binding:2 ~resource:(resource gpuMatrixC) ()
        ]
      |> array_of_list
    in
    let descriptor =
      GPUBindGroupDescriptor.create ~layout:bind_group_layout ~entries ()
    in
    device##createBindGroup ~descriptor
  in
  let compute_pipeline =
    let module_ =
      let code = Union.of_a code in
      let descriptor = GPUShaderModuleDescriptor.create ~code () in
      device##createShaderModule ~descriptor
    in
    let computeStage =
      GPUProgrammableStageDescriptor.create
        ~module_
        ~entryPoint:(Js.string "main")
        ()
    in
    let descriptor =
      GPUComputePipelineDescriptor.create
        ~computeStage
        ~layout:pipeline_layout
        ()
    in
    device##createComputePipeline ~descriptor
  in
  let commandEncoder =
    device##createCommandEncoder ~descriptor:Js.undefined
  in
  let passEncoder =
    commandEncoder##beginComputePass ~descriptor:Js.undefined
  in
  passEncoder##setPipeline ~pipeline:compute_pipeline;
  passEncoder##setBindGroup
    ~index:0
    ~bindGroup
    ~dynamicOffsets:Js.undefined;
  passEncoder##dispatch
    ~x:(matrixDimension / localSize)
    ~y:(Js.def (matrixDimension / localSize))
    ~z:Js.undefined;
  passEncoder##endPass;
  let gpuReadBuffer =
    create_buffer device ~usage:GPUBufferUsage.(_COPY_DST lor _MAP_READ)
  in
  commandEncoder##copyBufferToBuffer
    ~source:gpuMatrixC
    ~sourceOffset:0
    ~destination:gpuReadBuffer
    ~destinationOffset:0
    ~size:matrixSize;
  let gpuCommands = commandEncoder##finish ~descriptor:Js.undefined in
  device##.defaultQueue##submit ~buffers:(array_of_list [ gpuCommands ]);
  let%bind cpuMatrixC =
    gpuReadBuffer##mapReadAsync |> Lwt_promise.to_lwt
  in
  let result = new%js Typed_array.float32Array_fromBuffer cpuMatrixC in
  return result
;;

let computeOnCpu matrixA matrixB =
  let result = new%js Typed_array.float32Array matrixElements in
  for resultX = 0 to matrixDimension - 1 do
    for resultY = 0 to matrixDimension - 1 do
      let sum = ref 0. in
      for i = 0 to matrixDimension - 1 do
        let aCell = i + (resultX * matrixDimension) in
        let bCell = resultY + (i * matrixDimension) in
        sum
          := Float.(
               !sum
               + (Typed_array.unsafe_get matrixA aCell
                 * Typed_array.unsafe_get matrixB bCell))
      done;
      let resultCell = resultY + (resultX * matrixDimension) in
      Typed_array.set result resultCell !sum
    done
  done;
  result
;;

let check_result ~gpuResult ~cpuResult =
  let correct = ref true in
  for i = 0 to matrixElements - 1 do
    let open Float in
    if abs
         (1.0
         - (Typed_array.unsafe_get gpuResult i
           / Typed_array.unsafe_get cpuResult i))
       > 0.00001
    then correct := false
  done;
  !correct
;;

let () =
  top_level (fun () ->
      let matrixA = random_floats () in
      let matrixB = random_floats () in
      debug_o matrixA;
      debug_o matrixB;
      let%bind gpuResult = computeOnGpu matrixA matrixB in
      debug_o gpuResult;
      let cpuResult = computeOnCpu matrixA matrixB in
      debug_o cpuResult;
      let correct = check_result ~gpuResult ~cpuResult in
      debug [%message (correct : bool)];
      return ())
;;
