
Documentation: https://tari3x.github.io/webgpu

Contains two opam packages:

js_of_ocaml-webidl
------------------

A tool to generate js-of-ocaml bindings from WebIDL. Incomplete, but sufficient
to translate the WebGPU standard, see js_of_ocaml-webgpu.

Includes a copy of the webidl package due to
https://github.com/0zat/webidl/issues/3

js_of_ocaml-webgpu
------------------

Js_of_ocaml bindings for WebGPU (https://gpuweb.github.io/gpuweb/)

The example is the rewrite in ocaml of
https://hello-webgpu-compute.glitch.me/hello-compute-chromium.html.
As of June 2020 you need firefox-nightly with dom.webgpu.enabled=true to
run the example. You'll also need to install Vulkan if running on Linux.
