open Js_of_ocaml
open Js
open Js.Unsafe
open Typed_array
open Js_of_ocaml_webidl.Runtime

type i32 = int
type u32 = int
type u64 = int

module GPUColorDict : sig
  class type js =
    object
      method r : float readonly_prop

      method g : float readonly_prop

      method b : float readonly_prop

      method a : float readonly_prop
    end

  type t = js Js.t

  val create : r:float -> g:float -> b:float -> a:float -> unit -> t
end

module GPUOrigin2DDict : sig
  class type js =
    object
      method x : u32 optdef readonly_prop

      method y : u32 optdef readonly_prop
    end

  type t = js Js.t

  val create : ?x:u32 -> ?y:u32 -> unit -> t
end

module GPUOrigin3DDict : sig
  class type js =
    object
      method x : u32 optdef readonly_prop

      method y : u32 optdef readonly_prop

      method z : u32 optdef readonly_prop
    end

  type t = js Js.t

  val create : ?x:u32 -> ?y:u32 -> ?z:u32 -> unit -> t
end

module GPUExtent3DDict : sig
  class type js =
    object
      method width : u32 readonly_prop

      method height : u32 readonly_prop

      method depth : u32 readonly_prop
    end

  type t = js Js.t

  val create : width:u32 -> height:u32 -> depth:u32 -> unit -> t
end

module GPUColor : sig
  type t = (float js_array Js.t, GPUColorDict.t) Union.t Js.t
end

module GPUOrigin2D : sig
  type t = (u32 js_array Js.t, GPUOrigin2DDict.t) Union.t Js.t
end

module GPUOrigin3D : sig
  type t = (u32 js_array Js.t, GPUOrigin3DDict.t) Union.t Js.t
end

module GPUExtent3D : sig
  type t = (u32 js_array Js.t, GPUExtent3DDict.t) Union.t Js.t
end

module GPUObjectBase : sig
  type witness

  class type js =
    object
      method gpuobjectbase_witness : witness

      method label : js_string Js.t prop
    end

  type t = js Js.t
end

module GPUObjectDescriptorBase : sig
  class type js =
    object
      method label : js_string Js.t optdef readonly_prop
    end

  type t = js Js.t

  val create : ?label:js_string Js.t -> unit -> t
end

module GPUPowerPreference : sig
  type t = js_string Js.t
end

module GPURequestAdapterOptions : sig
  class type js =
    object
      method powerPreference : GPUPowerPreference.t optdef readonly_prop
    end

  type t = js Js.t

  val create : ?powerPreference:GPUPowerPreference.t -> unit -> t
end

module GPUBufferUsageFlags : sig
  type t = u32
end

module GPUBufferDescriptor : sig
  class type js =
    object
      method size : u64 readonly_prop

      method usage : GPUBufferUsageFlags.t readonly_prop
    end

  type t = js Js.t

  val create : size:u64 -> usage:GPUBufferUsageFlags.t -> unit -> t
end

module GPUBuffer : sig
  type witness

  class type js =
    object
      method gpubuffer_witness : witness

      inherit GPUObjectBase.js

      method mapReadAsync : arrayBuffer Js.t Promise.t Js.t meth

      method unmap : unit meth
    end

  type t = js Js.t
end

module GPUMappedBuffer : sig
  type t = any js_array Js.t
end

module GPUTextureDimension : sig
  type t = js_string Js.t
end

module GPUTextureFormat : sig
  type t = js_string Js.t
end

module GPUTextureUsageFlags : sig
  type t = u32
end

module GPUTextureDescriptor : sig
  class type js =
    object
      method size : GPUExtent3D.t readonly_prop

      method mipLevelCount : u32 optdef readonly_prop

      method sampleCount : u32 optdef readonly_prop

      method dimension : GPUTextureDimension.t optdef readonly_prop

      method format : GPUTextureFormat.t readonly_prop

      method usage : GPUTextureUsageFlags.t readonly_prop
    end

  type t = js Js.t

  val create
    :  size:GPUExtent3D.t
    -> ?mipLevelCount:u32
    -> ?sampleCount:u32
    -> ?dimension:GPUTextureDimension.t
    -> format:GPUTextureFormat.t
    -> usage:GPUTextureUsageFlags.t
    -> unit
    -> t
end

module GPUTextureViewDimension : sig
  type t = js_string Js.t
end

module GPUTextureAspect : sig
  type t = js_string Js.t
end

module GPUTextureViewDescriptor : sig
  class type js =
    object
      inherit GPUObjectDescriptorBase.js

      method format : GPUTextureFormat.t optdef readonly_prop

      method dimension : GPUTextureViewDimension.t optdef readonly_prop

      method aspect : GPUTextureAspect.t optdef readonly_prop

      method baseMipLevel : u32 optdef readonly_prop

      method mipLevelCount : u32 optdef readonly_prop

      method baseArrayLayer : u32 optdef readonly_prop

      method arrayLayerCount : u32 optdef readonly_prop
    end

  type t = js Js.t

  val create
    :  ?label:js_string Js.t
    -> ?format:GPUTextureFormat.t
    -> ?dimension:GPUTextureViewDimension.t
    -> ?aspect:GPUTextureAspect.t
    -> ?baseMipLevel:u32
    -> ?mipLevelCount:u32
    -> ?baseArrayLayer:u32
    -> ?arrayLayerCount:u32
    -> unit
    -> t
end

module GPUTextureView : sig
  type witness

  class type js =
    object
      method gputextureview_witness : witness

      inherit GPUObjectBase.js
    end

  type t = js Js.t
end

module GPUTexture : sig
  type witness

  class type js =
    object
      method gputexture_witness : witness

      inherit GPUObjectBase.js

      method createView :
        descriptor:GPUTextureViewDescriptor.t optdef -> GPUTextureView.t meth
    end

  type t = js Js.t
end

module GPUAddressMode : sig
  type t = js_string Js.t
end

module GPUFilterMode : sig
  type t = js_string Js.t
end

module GPUCompareFunction : sig
  type t = js_string Js.t
end

module GPUSamplerDescriptor : sig
  class type js =
    object
      inherit GPUObjectDescriptorBase.js

      method addressModeU : GPUAddressMode.t optdef readonly_prop

      method addressModeV : GPUAddressMode.t optdef readonly_prop

      method addressModeW : GPUAddressMode.t optdef readonly_prop

      method magFilter : GPUFilterMode.t optdef readonly_prop

      method minFilter : GPUFilterMode.t optdef readonly_prop

      method mipmapFilter : GPUFilterMode.t optdef readonly_prop

      method lodMinClamp : float optdef readonly_prop

      method lodMaxClamp : float optdef readonly_prop

      method compare : GPUCompareFunction.t optdef readonly_prop
    end

  type t = js Js.t

  val create
    :  ?label:js_string Js.t
    -> ?addressModeU:GPUAddressMode.t
    -> ?addressModeV:GPUAddressMode.t
    -> ?addressModeW:GPUAddressMode.t
    -> ?magFilter:GPUFilterMode.t
    -> ?minFilter:GPUFilterMode.t
    -> ?mipmapFilter:GPUFilterMode.t
    -> ?lodMinClamp:float
    -> ?lodMaxClamp:float
    -> ?compare:GPUCompareFunction.t
    -> unit
    -> t
end

module GPUSampler : sig
  type witness

  class type js =
    object
      method gpusampler_witness : witness

      inherit GPUObjectBase.js
    end

  type t = js Js.t
end

module GPUBindGroupLayout : sig
  type witness

  class type js =
    object
      method gpubindgrouplayout_witness : witness

      inherit GPUObjectBase.js
    end

  type t = js Js.t
end

module GPUPipelineLayoutDescriptor : sig
  class type js =
    object
      inherit GPUObjectDescriptorBase.js

      method bindGroupLayouts : GPUBindGroupLayout.t js_array Js.t readonly_prop
    end

  type t = js Js.t

  val create
    :  ?label:js_string Js.t
    -> bindGroupLayouts:GPUBindGroupLayout.t js_array Js.t
    -> unit
    -> t
end

module GPUPipelineLayout : sig
  type witness

  class type js =
    object
      method gpupipelinelayout_witness : witness

      inherit GPUObjectBase.js
    end

  type t = js Js.t
end

module GPUTextureComponentType : sig
  type t = js_string Js.t
end

module GPUShaderStageFlags : sig
  type t = u32
end

module GPUBindingType : sig
  type t = js_string Js.t
end

module GPUBindGroupLayoutEntry : sig
  class type js =
    object
      method binding : u32 readonly_prop

      method visibility : GPUShaderStageFlags.t readonly_prop

      method type_ : GPUBindingType.t readonly_prop

      method viewDimension : GPUTextureViewDimension.t optdef readonly_prop

      method textureComponentType : GPUTextureComponentType.t optdef readonly_prop

      method multisampled : bool Js.t optdef readonly_prop

      method hasDynamicOffset : bool Js.t optdef readonly_prop

      method storageTextureFormat : GPUTextureFormat.t optdef readonly_prop
    end

  type t = js Js.t

  val create
    :  binding:u32
    -> visibility:GPUShaderStageFlags.t
    -> type_:GPUBindingType.t
    -> ?viewDimension:GPUTextureViewDimension.t
    -> ?textureComponentType:GPUTextureComponentType.t
    -> ?multisampled:bool Js.t
    -> ?hasDynamicOffset:bool Js.t
    -> ?storageTextureFormat:GPUTextureFormat.t
    -> unit
    -> t
end

module GPUBindGroupLayoutDescriptor : sig
  class type js =
    object
      inherit GPUObjectDescriptorBase.js

      method entries : GPUBindGroupLayoutEntry.t js_array Js.t readonly_prop
    end

  type t = js Js.t

  val create
    :  ?label:js_string Js.t
    -> entries:GPUBindGroupLayoutEntry.t js_array Js.t
    -> unit
    -> t
end

module GPUBufferBinding : sig
  class type js =
    object
      method buffer : GPUBuffer.t readonly_prop

      method offset : u64 optdef readonly_prop

      method size : u64 optdef readonly_prop
    end

  type t = js Js.t

  val create : buffer:GPUBuffer.t -> ?offset:u64 -> ?size:u64 -> unit -> t
end

module GPUBindingResource : sig
  type t =
    (GPUSampler.t, (GPUTextureView.t, GPUBufferBinding.t) Union.t Js.t) Union.t Js.t
end

module GPUBindGroupEntry : sig
  class type js =
    object
      method binding : u32 readonly_prop

      method resource : GPUBindingResource.t readonly_prop
    end

  type t = js Js.t

  val create : binding:u32 -> resource:GPUBindingResource.t -> unit -> t
end

module GPUBindGroupDescriptor : sig
  class type js =
    object
      inherit GPUObjectDescriptorBase.js

      method layout : GPUBindGroupLayout.t readonly_prop

      method entries : GPUBindGroupEntry.t js_array Js.t readonly_prop
    end

  type t = js Js.t

  val create
    :  ?label:js_string Js.t
    -> layout:GPUBindGroupLayout.t
    -> entries:GPUBindGroupEntry.t js_array Js.t
    -> unit
    -> t
end

module GPUBindGroup : sig
  type witness

  class type js =
    object
      method gpubindgroup_witness : witness

      inherit GPUObjectBase.js
    end

  type t = js Js.t
end

module GPUShaderCode : sig
  type t = (uint32Array Js.t, js_string Js.t) Union.t Js.t
end

module GPUShaderModuleDescriptor : sig
  class type js =
    object
      inherit GPUObjectDescriptorBase.js

      method code : GPUShaderCode.t readonly_prop
    end

  type t = js Js.t

  val create : ?label:js_string Js.t -> code:GPUShaderCode.t -> unit -> t
end

module GPUShaderModule : sig
  type witness

  class type js =
    object
      method gpushadermodule_witness : witness

      inherit GPUObjectBase.js
    end

  type t = js Js.t
end

module GPUPipelineDescriptorBase : sig
  class type js =
    object
      inherit GPUObjectDescriptorBase.js

      method layout : GPUPipelineLayout.t readonly_prop
    end

  type t = js Js.t

  val create : ?label:js_string Js.t -> layout:GPUPipelineLayout.t -> unit -> t
end

module GPUProgrammableStageDescriptor : sig
  class type js =
    object
      method module_ : GPUShaderModule.t readonly_prop

      method entryPoint : js_string Js.t readonly_prop
    end

  type t = js Js.t

  val create : module_:GPUShaderModule.t -> entryPoint:js_string Js.t -> unit -> t
end

module GPUComputePipelineDescriptor : sig
  class type js =
    object
      inherit GPUPipelineDescriptorBase.js

      method computeStage : GPUProgrammableStageDescriptor.t readonly_prop
    end

  type t = js Js.t

  val create
    :  ?label:js_string Js.t
    -> layout:GPUPipelineLayout.t
    -> computeStage:GPUProgrammableStageDescriptor.t
    -> unit
    -> t
end

module GPUComputePipeline : sig
  type witness

  class type js =
    object
      method gpucomputepipeline_witness : witness

      inherit GPUObjectBase.js
    end

  type t = js Js.t
end

module GPUBlendFactor : sig
  type t = js_string Js.t
end

module GPUBlendOperation : sig
  type t = js_string Js.t
end

module GPUBlendDescriptor : sig
  class type js =
    object
      method srcFactor : GPUBlendFactor.t optdef readonly_prop

      method dstFactor : GPUBlendFactor.t optdef readonly_prop

      method operation : GPUBlendOperation.t optdef readonly_prop
    end

  type t = js Js.t

  val create
    :  ?srcFactor:GPUBlendFactor.t
    -> ?dstFactor:GPUBlendFactor.t
    -> ?operation:GPUBlendOperation.t
    -> unit
    -> t
end

module GPUColorWriteFlags : sig
  type t = u32
end

module GPUColorStateDescriptor : sig
  class type js =
    object
      method format : GPUTextureFormat.t readonly_prop

      method alpha : GPUBlendDescriptor.t optdef readonly_prop

      method color : GPUBlendDescriptor.t optdef readonly_prop

      method writeMask : GPUColorWriteFlags.t optdef readonly_prop
    end

  type t = js Js.t

  val create
    :  format:GPUTextureFormat.t
    -> ?alpha:GPUBlendDescriptor.t
    -> ?color:GPUBlendDescriptor.t
    -> ?writeMask:GPUColorWriteFlags.t
    -> unit
    -> t
end

module GPUStencilOperation : sig
  type t = js_string Js.t
end

module GPUStencilStateFaceDescriptor : sig
  class type js =
    object
      method compare : GPUCompareFunction.t optdef readonly_prop

      method failOp : GPUStencilOperation.t optdef readonly_prop

      method depthFailOp : GPUStencilOperation.t optdef readonly_prop

      method passOp : GPUStencilOperation.t optdef readonly_prop
    end

  type t = js Js.t

  val create
    :  ?compare:GPUCompareFunction.t
    -> ?failOp:GPUStencilOperation.t
    -> ?depthFailOp:GPUStencilOperation.t
    -> ?passOp:GPUStencilOperation.t
    -> unit
    -> t
end

module GPUDepthStencilStateDescriptor : sig
  class type js =
    object
      method format : GPUTextureFormat.t readonly_prop

      method depthWriteEnabled : bool Js.t optdef readonly_prop

      method depthCompare : GPUCompareFunction.t optdef readonly_prop

      method stencilFront : GPUStencilStateFaceDescriptor.t readonly_prop

      method stencilBack : GPUStencilStateFaceDescriptor.t readonly_prop

      method stencilReadMask : u32 optdef readonly_prop

      method stencilWriteMask : u32 optdef readonly_prop
    end

  type t = js Js.t

  val create
    :  format:GPUTextureFormat.t
    -> ?depthWriteEnabled:bool Js.t
    -> ?depthCompare:GPUCompareFunction.t
    -> stencilFront:GPUStencilStateFaceDescriptor.t
    -> stencilBack:GPUStencilStateFaceDescriptor.t
    -> ?stencilReadMask:u32
    -> ?stencilWriteMask:u32
    -> unit
    -> t
end

module GPUIndexFormat : sig
  type t = js_string Js.t
end

module GPUInputStepMode : sig
  type t = js_string Js.t
end

module GPUVertexFormat : sig
  type t = js_string Js.t
end

module GPUVertexAttributeDescriptor : sig
  class type js =
    object
      method offset : u64 optdef readonly_prop

      method format : GPUVertexFormat.t readonly_prop

      method shaderLocation : u32 readonly_prop
    end

  type t = js Js.t

  val create : ?offset:u64 -> format:GPUVertexFormat.t -> shaderLocation:u32 -> unit -> t
end

module GPUVertexBufferLayoutDescriptor : sig
  class type js =
    object
      method arrayStride : u64 readonly_prop

      method stepMode : GPUInputStepMode.t optdef readonly_prop

      method attributes : GPUVertexAttributeDescriptor.t js_array Js.t readonly_prop
    end

  type t = js Js.t

  val create
    :  arrayStride:u64
    -> ?stepMode:GPUInputStepMode.t
    -> attributes:GPUVertexAttributeDescriptor.t js_array Js.t
    -> unit
    -> t
end

module GPUVertexStateDescriptor : sig
  class type js =
    object
      method indexFormat : GPUIndexFormat.t optdef readonly_prop

      method vertexBuffers :
        GPUVertexBufferLayoutDescriptor.t js_array Js.t optdef readonly_prop
    end

  type t = js Js.t

  val create
    :  ?indexFormat:GPUIndexFormat.t
    -> ?vertexBuffers:GPUVertexBufferLayoutDescriptor.t js_array Js.t
    -> unit
    -> t
end

module GPUPrimitiveTopology : sig
  type t = js_string Js.t
end

module GPUFrontFace : sig
  type t = js_string Js.t
end

module GPUCullMode : sig
  type t = js_string Js.t
end

module GPURasterizationStateDescriptor : sig
  class type js =
    object
      method frontFace : GPUFrontFace.t optdef readonly_prop

      method cullMode : GPUCullMode.t optdef readonly_prop

      method depthBias : i32 optdef readonly_prop

      method depthBiasSlopeScale : float optdef readonly_prop

      method depthBiasClamp : float optdef readonly_prop
    end

  type t = js Js.t

  val create
    :  ?frontFace:GPUFrontFace.t
    -> ?cullMode:GPUCullMode.t
    -> ?depthBias:i32
    -> ?depthBiasSlopeScale:float
    -> ?depthBiasClamp:float
    -> unit
    -> t
end

module GPURenderPipelineDescriptor : sig
  class type js =
    object
      inherit GPUPipelineDescriptorBase.js

      method vertexStage : GPUProgrammableStageDescriptor.t readonly_prop

      method fragmentStage : GPUProgrammableStageDescriptor.t optdef readonly_prop

      method primitiveTopology : GPUPrimitiveTopology.t readonly_prop

      method rasterizationState : GPURasterizationStateDescriptor.t optdef readonly_prop

      method colorStates : GPUColorStateDescriptor.t js_array Js.t readonly_prop

      method depthStencilState : GPUDepthStencilStateDescriptor.t optdef readonly_prop

      method vertexState : GPUVertexStateDescriptor.t optdef readonly_prop

      method sampleCount : u32 optdef readonly_prop

      method sampleMask : u32 optdef readonly_prop

      method alphaToCoverageEnabled : bool Js.t optdef readonly_prop
    end

  type t = js Js.t

  val create
    :  ?label:js_string Js.t
    -> layout:GPUPipelineLayout.t
    -> vertexStage:GPUProgrammableStageDescriptor.t
    -> ?fragmentStage:GPUProgrammableStageDescriptor.t
    -> primitiveTopology:GPUPrimitiveTopology.t
    -> ?rasterizationState:GPURasterizationStateDescriptor.t
    -> colorStates:GPUColorStateDescriptor.t js_array Js.t
    -> ?depthStencilState:GPUDepthStencilStateDescriptor.t
    -> ?vertexState:GPUVertexStateDescriptor.t
    -> ?sampleCount:u32
    -> ?sampleMask:u32
    -> ?alphaToCoverageEnabled:bool Js.t
    -> unit
    -> t
end

module GPURenderPipeline : sig
  type witness

  class type js =
    object
      method gpurenderpipeline_witness : witness

      inherit GPUObjectBase.js
    end

  type t = js Js.t
end

module GPUCommandEncoderDescriptor : sig
  class type js =
    object
      inherit GPUObjectDescriptorBase.js
    end

  type t = js Js.t

  val create : ?label:js_string Js.t -> unit -> t
end

module GPULoadOp : sig
  type t = js_string Js.t
end

module GPUStoreOp : sig
  type t = js_string Js.t
end

module GPURenderPassColorAttachmentDescriptor : sig
  class type js =
    object
      method attachment : GPUTextureView.t readonly_prop

      method resolveTarget : GPUTextureView.t optdef readonly_prop

      method loadValue : (GPULoadOp.t, GPUColor.t) Union.t Js.t readonly_prop

      method storeOp : GPUStoreOp.t optdef readonly_prop
    end

  type t = js Js.t

  val create
    :  attachment:GPUTextureView.t
    -> ?resolveTarget:GPUTextureView.t
    -> loadValue:(GPULoadOp.t, GPUColor.t) Union.t Js.t
    -> ?storeOp:GPUStoreOp.t
    -> unit
    -> t
end

module GPURenderPassDepthStencilAttachmentDescriptor : sig
  class type js =
    object
      method attachment : GPUTextureView.t readonly_prop

      method depthLoadValue : (GPULoadOp.t, float) Union.t Js.t readonly_prop

      method depthStoreOp : GPUStoreOp.t readonly_prop

      method stencilLoadValue : (GPULoadOp.t, u32) Union.t Js.t readonly_prop

      method stencilStoreOp : GPUStoreOp.t readonly_prop
    end

  type t = js Js.t

  val create
    :  attachment:GPUTextureView.t
    -> depthLoadValue:(GPULoadOp.t, float) Union.t Js.t
    -> depthStoreOp:GPUStoreOp.t
    -> stencilLoadValue:(GPULoadOp.t, u32) Union.t Js.t
    -> stencilStoreOp:GPUStoreOp.t
    -> unit
    -> t
end

module GPURenderPassDescriptor : sig
  class type js =
    object
      inherit GPUObjectDescriptorBase.js

      method colorAttachments :
        GPURenderPassColorAttachmentDescriptor.t js_array Js.t readonly_prop

      method depthStencilAttachment :
        GPURenderPassDepthStencilAttachmentDescriptor.t optdef readonly_prop
    end

  type t = js Js.t

  val create
    :  ?label:js_string Js.t
    -> colorAttachments:GPURenderPassColorAttachmentDescriptor.t js_array Js.t
    -> ?depthStencilAttachment:GPURenderPassDepthStencilAttachmentDescriptor.t
    -> unit
    -> t
end

module GPUBufferCopyView : sig
  class type js =
    object
      method buffer : GPUBuffer.t readonly_prop

      method offset : u64 optdef readonly_prop

      method bytesPerRow : u32 readonly_prop

      method rowsPerImage : u32 optdef readonly_prop
    end

  type t = js Js.t

  val create
    :  buffer:GPUBuffer.t
    -> ?offset:u64
    -> bytesPerRow:u32
    -> ?rowsPerImage:u32
    -> unit
    -> t
end

module GPUTextureCopyView : sig
  class type js =
    object
      method texture : GPUTexture.t readonly_prop

      method mipLevel : u32 optdef readonly_prop

      method arrayLayer : u32 optdef readonly_prop

      method origin : GPUOrigin3D.t optdef readonly_prop
    end

  type t = js Js.t

  val create
    :  texture:GPUTexture.t
    -> ?mipLevel:u32
    -> ?arrayLayer:u32
    -> ?origin:GPUOrigin3D.t
    -> unit
    -> t
end

module GPUProgrammablePassEncoder : sig
  type witness

  class type js =
    object
      method gpuprogrammablepassencoder_witness : witness

      method setBindGroup :
        index:int
        -> bindGroup:GPUBindGroup.t
        -> dynamicOffsets:int js_array Js.t optdef
        -> unit meth
    end

  type t = js Js.t
end

module GPURenderEncoderBase : sig
  type witness

  class type js =
    object
      method gpurenderencoderbase_witness : witness

      method setPipeline : pipeline:GPURenderPipeline.t -> unit meth

      method setIndexBuffer :
        buffer:GPUBuffer.t -> offset:u64 optdef -> size:u64 optdef -> unit meth

      method setVertexBuffer :
        slot:u32
        -> buffer:GPUBuffer.t
        -> offset:u64 optdef
        -> size:u64 optdef
        -> unit meth

      method draw :
        vertexCount:u32
        -> instanceCount:u32
        -> firstVertex:u32
        -> firstInstance:u32
        -> unit meth

      method drawIndexed :
        indexCount:u32
        -> instanceCount:u32
        -> firstIndex:u32
        -> baseVertex:i32
        -> firstInstance:u32
        -> unit meth
    end

  type t = js Js.t
end

module GPURenderPassEncoder : sig
  type witness

  class type js =
    object
      method gpurenderpassencoder_witness : witness

      inherit GPUObjectBase.js

      inherit GPUProgrammablePassEncoder.js

      inherit GPURenderEncoderBase.js

      method endPass : unit meth
    end

  type t = js Js.t
end

module GPUComputePassDescriptor : sig
  class type js =
    object
      inherit GPUObjectDescriptorBase.js
    end

  type t = js Js.t

  val create : ?label:js_string Js.t -> unit -> t
end

module GPUComputePassEncoder : sig
  type witness

  class type js =
    object
      method gpucomputepassencoder_witness : witness

      inherit GPUObjectBase.js

      inherit GPUProgrammablePassEncoder.js

      method setPipeline : pipeline:GPUComputePipeline.t -> unit meth

      method dispatch : x:u32 -> y:u32 optdef -> z:u32 optdef -> unit meth

      method endPass : unit meth
    end

  type t = js Js.t
end

module GPUCommandBufferDescriptor : sig
  class type js =
    object
      inherit GPUObjectDescriptorBase.js
    end

  type t = js Js.t

  val create : ?label:js_string Js.t -> unit -> t
end

module GPUCommandBuffer : sig
  type witness

  class type js =
    object
      method gpucommandbuffer_witness : witness

      inherit GPUObjectBase.js
    end

  type t = js Js.t
end

module GPUCommandEncoder : sig
  type witness

  class type js =
    object
      method gpucommandencoder_witness : witness

      inherit GPUObjectBase.js

      method beginComputePass :
        descriptor:GPUComputePassDescriptor.t optdef -> GPUComputePassEncoder.t meth

      method beginRenderPass :
        descriptor:GPURenderPassDescriptor.t -> GPURenderPassEncoder.t meth

      method copyBufferToBuffer :
        source:GPUBuffer.t
        -> sourceOffset:u64
        -> destination:GPUBuffer.t
        -> destinationOffset:u64
        -> size:u64
        -> unit meth

      method copyBufferToTexture :
        source:GPUBufferCopyView.t
        -> destination:GPUTextureCopyView.t
        -> copySize:GPUExtent3D.t
        -> unit meth

      method copyTextureToBuffer :
        source:GPUTextureCopyView.t
        -> destination:GPUBufferCopyView.t
        -> copySize:GPUExtent3D.t
        -> unit meth

      method copyTextureToTexture :
        source:GPUTextureCopyView.t
        -> destination:GPUTextureCopyView.t
        -> copySize:GPUExtent3D.t
        -> unit meth

      method finish :
        descriptor:GPUCommandBufferDescriptor.t optdef -> GPUCommandBuffer.t meth
    end

  type t = js Js.t
end

module GPUQueue : sig
  type witness

  class type js =
    object
      method gpuqueue_witness : witness

      inherit GPUObjectBase.js

      method submit : buffers:GPUCommandBuffer.t js_array Js.t -> unit meth
    end

  type t = js Js.t
end

module GPUDevice : sig
  type witness

  class type js =
    object
      method gpudevice_witness : witness

      inherit GPUObjectBase.js

      method defaultQueue : GPUQueue.t readonly_prop

      method createBuffer : descriptor:GPUBufferDescriptor.t -> GPUBuffer.t meth

      method createBufferMapped :
        descriptor:GPUBufferDescriptor.t -> GPUMappedBuffer.t meth

      method createTexture : descriptor:GPUTextureDescriptor.t -> GPUTexture.t meth

      method createSampler : descriptor:GPUSamplerDescriptor.t optdef -> GPUSampler.t meth

      method createBindGroupLayout :
        descriptor:GPUBindGroupLayoutDescriptor.t -> GPUBindGroupLayout.t meth

      method createPipelineLayout :
        descriptor:GPUPipelineLayoutDescriptor.t -> GPUPipelineLayout.t meth

      method createBindGroup : descriptor:GPUBindGroupDescriptor.t -> GPUBindGroup.t meth

      method createShaderModule :
        descriptor:GPUShaderModuleDescriptor.t -> GPUShaderModule.t meth

      method createComputePipeline :
        descriptor:GPUComputePipelineDescriptor.t -> GPUComputePipeline.t meth

      method createRenderPipeline :
        descriptor:GPURenderPipelineDescriptor.t -> GPURenderPipeline.t meth

      method createCommandEncoder :
        descriptor:GPUCommandEncoderDescriptor.t optdef -> GPUCommandEncoder.t meth
    end

  type t = js Js.t
end

module GPUExtensions : sig
  class type js =
    object
      method anisotropicFiltering : bool Js.t optdef readonly_prop
    end

  type t = js Js.t

  val create : ?anisotropicFiltering:bool Js.t -> unit -> t
end

module GPULimits : sig
  class type js =
    object
      method maxBindGroups : u32 optdef readonly_prop
    end

  type t = js Js.t

  val create : ?maxBindGroups:u32 -> unit -> t
end

module GPUDeviceDescriptor : sig
  class type js =
    object
      method extensions : GPUExtensions.t optdef readonly_prop

      method limits : GPULimits.t optdef readonly_prop
    end

  type t = js Js.t

  val create : ?extensions:GPUExtensions.t -> ?limits:GPULimits.t -> unit -> t
end

module GPUAdapter : sig
  type witness

  class type js =
    object
      method gpuadapter_witness : witness

      inherit GPUObjectBase.js

      method name : js_string Js.t readonly_prop

      method requestDevice :
        descriptor:GPUDeviceDescriptor.t optdef -> GPUDevice.t Promise.t Js.t meth
    end

  type t = js Js.t
end

module GPU : sig
  type witness

  class type js =
    object
      method gpu_witness : witness

      method requestAdapter :
        options:GPURequestAdapterOptions.t optdef -> GPUAdapter.t Promise.t Js.t meth
    end

  type t = js Js.t
end

module GPUProvider : sig
  type witness

  class type js =
    object
      method gpuprovider_witness : witness

      method gpu : GPU.t readonly_prop
    end

  type t = js Js.t
end

module GPUDeviceLostInfo : sig
  type witness

  class type js =
    object
      method gpudevicelostinfo_witness : witness

      method message : js_string Js.t readonly_prop
    end

  type t = js Js.t
end

module GPUErrorFilter : sig
  type t = js_string Js.t
end

module GPUOutOfMemoryError : sig
  type witness

  class type js =
    object
      method gpuoutofmemoryerror_witness : witness
    end

  type t = js Js.t
end

module GPUValidationError : sig
  type witness

  class type js =
    object
      method gpuvalidationerror_witness : witness
    end

  type t = js Js.t
end

module GPUError : sig
  type t = (GPUOutOfMemoryError.t, GPUValidationError.t) Union.t Js.t
end

module GPUBufferUsage : sig
  type witness

  class type js =
    object
      method gpubufferusage_witness : witness
    end

  type t = js Js.t

  val _NONE : u32
  val _MAP_READ : u32
  val _MAP_WRITE : u32
  val _COPY_SRC : u32
  val _COPY_DST : u32
  val _INDEX : u32
  val _VERTEX : u32
  val _UNIFORM : u32
  val _STORAGE : u32
  val _INDIRECT : u32
end

module GPUTextureUsage : sig
  type witness

  class type js =
    object
      method gputextureusage_witness : witness
    end

  type t = js Js.t

  val _NONE : u32
  val _COPY_SRC : u32
  val _COPY_DST : u32
  val _SAMPLED : u32
  val _STORAGE : u32
  val _OUTPUT_ATTACHMENT : u32
end

module GPUShaderStage : sig
  type witness

  class type js =
    object
      method gpushaderstage_witness : witness
    end

  type t = js Js.t

  val _NONE : u32
  val _VERTEX : u32
  val _FRAGMENT : u32
  val _COMPUTE : u32
end

module GPUColorWrite : sig
  type witness

  class type js =
    object
      method gpucolorwrite_witness : witness
    end

  type t = js Js.t

  val _NONE : u32
  val _RED : u32
  val _GREEN : u32
  val _BLUE : u32
  val _ALPHA : u32
  val _ALL : u32
end

module GPUImageBitmapCopyView : sig
  class type js =
    object
      method origin : GPUOrigin2D.t optdef readonly_prop
    end

  type t = js Js.t

  val create : ?origin:GPUOrigin2D.t -> unit -> t
end

module GPURenderBundleEncoderDescriptor : sig
  class type js =
    object
      inherit GPUObjectDescriptorBase.js

      method colorFormats : GPUTextureFormat.t js_array Js.t readonly_prop

      method depthStencilFormat : GPUTextureFormat.t optdef readonly_prop

      method sampleCount : u32 optdef readonly_prop
    end

  type t = js Js.t

  val create
    :  ?label:js_string Js.t
    -> colorFormats:GPUTextureFormat.t js_array Js.t
    -> ?depthStencilFormat:GPUTextureFormat.t
    -> ?sampleCount:u32
    -> unit
    -> t
end

module GPURenderBundleEncoder : sig
  type witness

  class type js =
    object
      method gpurenderbundleencoder_witness : witness

      inherit GPUObjectBase.js
    end

  type t = js Js.t
end

module GPURenderBundleDescriptor : sig
  class type js =
    object
      inherit GPUObjectDescriptorBase.js
    end

  type t = js Js.t

  val create : ?label:js_string Js.t -> unit -> t
end

module GPURenderBundle : sig
  type witness

  class type js =
    object
      method gpurenderbundle_witness : witness

      inherit GPUObjectBase.js
    end

  type t = js Js.t
end

module GPUFenceDescriptor : sig
  class type js =
    object
      inherit GPUObjectDescriptorBase.js

      method initialValue : u64 optdef readonly_prop
    end

  type t = js Js.t

  val create : ?label:js_string Js.t -> ?initialValue:u64 -> unit -> t
end

module GPUFence : sig
  type witness

  class type js =
    object
      method gpufence_witness : witness

      inherit GPUObjectBase.js
    end

  type t = js Js.t
end

module GPUSwapChain : sig
  type witness

  class type js =
    object
      method gpuswapchain_witness : witness

      inherit GPUObjectBase.js

      method getCurrentTexture : GPUTexture.t meth
    end

  type t = js Js.t
end

module GPUSwapChainDescriptor : sig
  class type js =
    object
      inherit GPUObjectDescriptorBase.js

      method device : GPUDevice.t readonly_prop

      method format : GPUTextureFormat.t readonly_prop

      method usage : GPUTextureUsageFlags.t optdef readonly_prop
    end

  type t = js Js.t

  val create
    :  ?label:js_string Js.t
    -> device:GPUDevice.t
    -> format:GPUTextureFormat.t
    -> ?usage:GPUTextureUsageFlags.t
    -> unit
    -> t
end

module GPUCanvasContext : sig
  type witness

  class type js =
    object
      method gpucanvascontext_witness : witness

      method configureSwapChain :
        descriptor:GPUSwapChainDescriptor.t -> GPUSwapChain.t meth
    end

  type t = js Js.t
end
