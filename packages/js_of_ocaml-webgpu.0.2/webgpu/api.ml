open Js_of_ocaml
open Js
open Js.Unsafe
open Typed_array
open Js_of_ocaml_webidl.Runtime

type i32 = int
type u32 = int
type u64 = int

module GPUColorDict = struct
  class type js =
    object
      method r : float readonly_prop

      method g : float readonly_prop

      method b : float readonly_prop

      method a : float readonly_prop
    end

  type t = js Js.t

  let create ~r ~g ~b ~a () =
    object%js
      val r = r

      val g = g

      val b = b

      val a = a
    end
  ;;
end

module GPUOrigin2DDict = struct
  class type js =
    object
      method x : u32 optdef readonly_prop

      method y : u32 optdef readonly_prop
    end

  type t = js Js.t

  let create ?x ?y () =
    object%js
      val x = Optdef.option x

      val y = Optdef.option y
    end
  ;;
end

module GPUOrigin3DDict = struct
  class type js =
    object
      method x : u32 optdef readonly_prop

      method y : u32 optdef readonly_prop

      method z : u32 optdef readonly_prop
    end

  type t = js Js.t

  let create ?x ?y ?z () =
    object%js
      val x = Optdef.option x

      val y = Optdef.option y

      val z = Optdef.option z
    end
  ;;
end

module GPUExtent3DDict = struct
  class type js =
    object
      method width : u32 readonly_prop

      method height : u32 readonly_prop

      method depth : u32 readonly_prop
    end

  type t = js Js.t

  let create ~width ~height ~depth () =
    object%js
      val width = width

      val height = height

      val depth = depth
    end
  ;;
end

module GPUColor = struct
  type t = (float js_array Js.t, GPUColorDict.t) Union.t Js.t
end

module GPUOrigin2D = struct
  type t = (u32 js_array Js.t, GPUOrigin2DDict.t) Union.t Js.t
end

module GPUOrigin3D = struct
  type t = (u32 js_array Js.t, GPUOrigin3DDict.t) Union.t Js.t
end

module GPUExtent3D = struct
  type t = (u32 js_array Js.t, GPUExtent3DDict.t) Union.t Js.t
end

module GPUObjectBase = struct
  type witness

  class type js =
    object
      method gpuobjectbase_witness : witness

      method label : js_string Js.t prop
    end

  type t = js Js.t
end

module GPUObjectDescriptorBase = struct
  class type js =
    object
      method label : js_string Js.t optdef readonly_prop
    end

  type t = js Js.t

  let create ?label () =
    object%js
      val label = Optdef.option label
    end
  ;;
end

module GPUPowerPreference = struct
  type t = js_string Js.t
end

module GPURequestAdapterOptions = struct
  class type js =
    object
      method powerPreference : GPUPowerPreference.t optdef readonly_prop
    end

  type t = js Js.t

  let create ?powerPreference () =
    object%js
      val powerPreference = Optdef.option powerPreference
    end
  ;;
end

module GPUBufferUsageFlags = struct
  type t = u32
end

module GPUBufferDescriptor = struct
  class type js =
    object
      method size : u64 readonly_prop

      method usage : GPUBufferUsageFlags.t readonly_prop
    end

  type t = js Js.t

  let create ~size ~usage () =
    object%js
      val size = size

      val usage = usage
    end
  ;;
end

module GPUBuffer = struct
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

module GPUMappedBuffer = struct
  type t = any js_array Js.t
end

module GPUTextureDimension = struct
  type t = js_string Js.t
end

module GPUTextureFormat = struct
  type t = js_string Js.t
end

module GPUTextureUsageFlags = struct
  type t = u32
end

module GPUTextureDescriptor = struct
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

  let create ~size ?mipLevelCount ?sampleCount ?dimension ~format ~usage () =
    object%js
      val size = size

      val mipLevelCount = Optdef.option mipLevelCount

      val sampleCount = Optdef.option sampleCount

      val dimension = Optdef.option dimension

      val format = format

      val usage = usage
    end
  ;;
end

module GPUTextureViewDimension = struct
  type t = js_string Js.t
end

module GPUTextureAspect = struct
  type t = js_string Js.t
end

module GPUTextureViewDescriptor = struct
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

  let create
      ?label
      ?format
      ?dimension
      ?aspect
      ?baseMipLevel
      ?mipLevelCount
      ?baseArrayLayer
      ?arrayLayerCount
      ()
    =
    object%js
      val label = Optdef.option label

      val format = Optdef.option format

      val dimension = Optdef.option dimension

      val aspect = Optdef.option aspect

      val baseMipLevel = Optdef.option baseMipLevel

      val mipLevelCount = Optdef.option mipLevelCount

      val baseArrayLayer = Optdef.option baseArrayLayer

      val arrayLayerCount = Optdef.option arrayLayerCount
    end
  ;;
end

module GPUTextureView = struct
  type witness

  class type js =
    object
      method gputextureview_witness : witness

      inherit GPUObjectBase.js
    end

  type t = js Js.t
end

module GPUTexture = struct
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

module GPUAddressMode = struct
  type t = js_string Js.t
end

module GPUFilterMode = struct
  type t = js_string Js.t
end

module GPUCompareFunction = struct
  type t = js_string Js.t
end

module GPUSamplerDescriptor = struct
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

  let create
      ?label
      ?addressModeU
      ?addressModeV
      ?addressModeW
      ?magFilter
      ?minFilter
      ?mipmapFilter
      ?lodMinClamp
      ?lodMaxClamp
      ?compare
      ()
    =
    object%js
      val label = Optdef.option label

      val addressModeU = Optdef.option addressModeU

      val addressModeV = Optdef.option addressModeV

      val addressModeW = Optdef.option addressModeW

      val magFilter = Optdef.option magFilter

      val minFilter = Optdef.option minFilter

      val mipmapFilter = Optdef.option mipmapFilter

      val lodMinClamp = Optdef.option lodMinClamp

      val lodMaxClamp = Optdef.option lodMaxClamp

      val compare = Optdef.option compare
    end
  ;;
end

module GPUSampler = struct
  type witness

  class type js =
    object
      method gpusampler_witness : witness

      inherit GPUObjectBase.js
    end

  type t = js Js.t
end

module GPUBindGroupLayout = struct
  type witness

  class type js =
    object
      method gpubindgrouplayout_witness : witness

      inherit GPUObjectBase.js
    end

  type t = js Js.t
end

module GPUPipelineLayoutDescriptor = struct
  class type js =
    object
      inherit GPUObjectDescriptorBase.js

      method bindGroupLayouts : GPUBindGroupLayout.t js_array Js.t readonly_prop
    end

  type t = js Js.t

  let create ?label ~bindGroupLayouts () =
    object%js
      val label = Optdef.option label

      val bindGroupLayouts = bindGroupLayouts
    end
  ;;
end

module GPUPipelineLayout = struct
  type witness

  class type js =
    object
      method gpupipelinelayout_witness : witness

      inherit GPUObjectBase.js
    end

  type t = js Js.t
end

module GPUTextureComponentType = struct
  type t = js_string Js.t
end

module GPUShaderStageFlags = struct
  type t = u32
end

module GPUBindingType = struct
  type t = js_string Js.t
end

module GPUBindGroupLayoutEntry = struct
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

  let create
      ~binding
      ~visibility
      ~type_
      ?viewDimension
      ?textureComponentType
      ?multisampled
      ?hasDynamicOffset
      ?storageTextureFormat
      ()
    =
    object%js
      val binding = binding

      val visibility = visibility

      val type_ = type_

      val viewDimension = Optdef.option viewDimension

      val textureComponentType = Optdef.option textureComponentType

      val multisampled = Optdef.option multisampled

      val hasDynamicOffset = Optdef.option hasDynamicOffset

      val storageTextureFormat = Optdef.option storageTextureFormat
    end
  ;;
end

module GPUBindGroupLayoutDescriptor = struct
  class type js =
    object
      inherit GPUObjectDescriptorBase.js

      method entries : GPUBindGroupLayoutEntry.t js_array Js.t readonly_prop
    end

  type t = js Js.t

  let create ?label ~entries () =
    object%js
      val label = Optdef.option label

      val entries = entries
    end
  ;;
end

module GPUBufferBinding = struct
  class type js =
    object
      method buffer : GPUBuffer.t readonly_prop

      method offset : u64 optdef readonly_prop

      method size : u64 optdef readonly_prop
    end

  type t = js Js.t

  let create ~buffer ?offset ?size () =
    object%js
      val buffer = buffer

      val offset = Optdef.option offset

      val size = Optdef.option size
    end
  ;;
end

module GPUBindingResource = struct
  type t =
    (GPUSampler.t, (GPUTextureView.t, GPUBufferBinding.t) Union.t Js.t) Union.t Js.t
end

module GPUBindGroupEntry = struct
  class type js =
    object
      method binding : u32 readonly_prop

      method resource : GPUBindingResource.t readonly_prop
    end

  type t = js Js.t

  let create ~binding ~resource () =
    object%js
      val binding = binding

      val resource = resource
    end
  ;;
end

module GPUBindGroupDescriptor = struct
  class type js =
    object
      inherit GPUObjectDescriptorBase.js

      method layout : GPUBindGroupLayout.t readonly_prop

      method entries : GPUBindGroupEntry.t js_array Js.t readonly_prop
    end

  type t = js Js.t

  let create ?label ~layout ~entries () =
    object%js
      val label = Optdef.option label

      val layout = layout

      val entries = entries
    end
  ;;
end

module GPUBindGroup = struct
  type witness

  class type js =
    object
      method gpubindgroup_witness : witness

      inherit GPUObjectBase.js
    end

  type t = js Js.t
end

module GPUShaderCode = struct
  type t = (uint32Array Js.t, js_string Js.t) Union.t Js.t
end

module GPUShaderModuleDescriptor = struct
  class type js =
    object
      inherit GPUObjectDescriptorBase.js

      method code : GPUShaderCode.t readonly_prop
    end

  type t = js Js.t

  let create ?label ~code () =
    object%js
      val label = Optdef.option label

      val code = code
    end
  ;;
end

module GPUShaderModule = struct
  type witness

  class type js =
    object
      method gpushadermodule_witness : witness

      inherit GPUObjectBase.js
    end

  type t = js Js.t
end

module GPUPipelineDescriptorBase = struct
  class type js =
    object
      inherit GPUObjectDescriptorBase.js

      method layout : GPUPipelineLayout.t readonly_prop
    end

  type t = js Js.t

  let create ?label ~layout () =
    object%js
      val label = Optdef.option label

      val layout = layout
    end
  ;;
end

module GPUProgrammableStageDescriptor = struct
  class type js =
    object
      method module_ : GPUShaderModule.t readonly_prop

      method entryPoint : js_string Js.t readonly_prop
    end

  type t = js Js.t

  let create ~module_ ~entryPoint () =
    object%js
      val module_ = module_

      val entryPoint = entryPoint
    end
  ;;
end

module GPUComputePipelineDescriptor = struct
  class type js =
    object
      inherit GPUPipelineDescriptorBase.js

      method computeStage : GPUProgrammableStageDescriptor.t readonly_prop
    end

  type t = js Js.t

  let create ?label ~layout ~computeStage () =
    object%js
      val label = Optdef.option label

      val layout = layout

      val computeStage = computeStage
    end
  ;;
end

module GPUComputePipeline = struct
  type witness

  class type js =
    object
      method gpucomputepipeline_witness : witness

      inherit GPUObjectBase.js
    end

  type t = js Js.t
end

module GPUBlendFactor = struct
  type t = js_string Js.t
end

module GPUBlendOperation = struct
  type t = js_string Js.t
end

module GPUBlendDescriptor = struct
  class type js =
    object
      method srcFactor : GPUBlendFactor.t optdef readonly_prop

      method dstFactor : GPUBlendFactor.t optdef readonly_prop

      method operation : GPUBlendOperation.t optdef readonly_prop
    end

  type t = js Js.t

  let create ?srcFactor ?dstFactor ?operation () =
    object%js
      val srcFactor = Optdef.option srcFactor

      val dstFactor = Optdef.option dstFactor

      val operation = Optdef.option operation
    end
  ;;
end

module GPUColorWriteFlags = struct
  type t = u32
end

module GPUColorStateDescriptor = struct
  class type js =
    object
      method format : GPUTextureFormat.t readonly_prop

      method alpha : GPUBlendDescriptor.t optdef readonly_prop

      method color : GPUBlendDescriptor.t optdef readonly_prop

      method writeMask : GPUColorWriteFlags.t optdef readonly_prop
    end

  type t = js Js.t

  let create ~format ?alpha ?color ?writeMask () =
    object%js
      val format = format

      val alpha = Optdef.option alpha

      val color = Optdef.option color

      val writeMask = Optdef.option writeMask
    end
  ;;
end

module GPUStencilOperation = struct
  type t = js_string Js.t
end

module GPUStencilStateFaceDescriptor = struct
  class type js =
    object
      method compare : GPUCompareFunction.t optdef readonly_prop

      method failOp : GPUStencilOperation.t optdef readonly_prop

      method depthFailOp : GPUStencilOperation.t optdef readonly_prop

      method passOp : GPUStencilOperation.t optdef readonly_prop
    end

  type t = js Js.t

  let create ?compare ?failOp ?depthFailOp ?passOp () =
    object%js
      val compare = Optdef.option compare

      val failOp = Optdef.option failOp

      val depthFailOp = Optdef.option depthFailOp

      val passOp = Optdef.option passOp
    end
  ;;
end

module GPUDepthStencilStateDescriptor = struct
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

  let create
      ~format
      ?depthWriteEnabled
      ?depthCompare
      ~stencilFront
      ~stencilBack
      ?stencilReadMask
      ?stencilWriteMask
      ()
    =
    object%js
      val format = format

      val depthWriteEnabled = Optdef.option depthWriteEnabled

      val depthCompare = Optdef.option depthCompare

      val stencilFront = stencilFront

      val stencilBack = stencilBack

      val stencilReadMask = Optdef.option stencilReadMask

      val stencilWriteMask = Optdef.option stencilWriteMask
    end
  ;;
end

module GPUIndexFormat = struct
  type t = js_string Js.t
end

module GPUInputStepMode = struct
  type t = js_string Js.t
end

module GPUVertexFormat = struct
  type t = js_string Js.t
end

module GPUVertexAttributeDescriptor = struct
  class type js =
    object
      method offset : u64 optdef readonly_prop

      method format : GPUVertexFormat.t readonly_prop

      method shaderLocation : u32 readonly_prop
    end

  type t = js Js.t

  let create ?offset ~format ~shaderLocation () =
    object%js
      val offset = Optdef.option offset

      val format = format

      val shaderLocation = shaderLocation
    end
  ;;
end

module GPUVertexBufferLayoutDescriptor = struct
  class type js =
    object
      method arrayStride : u64 readonly_prop

      method stepMode : GPUInputStepMode.t optdef readonly_prop

      method attributes : GPUVertexAttributeDescriptor.t js_array Js.t readonly_prop
    end

  type t = js Js.t

  let create ~arrayStride ?stepMode ~attributes () =
    object%js
      val arrayStride = arrayStride

      val stepMode = Optdef.option stepMode

      val attributes = attributes
    end
  ;;
end

module GPUVertexStateDescriptor = struct
  class type js =
    object
      method indexFormat : GPUIndexFormat.t optdef readonly_prop

      method vertexBuffers :
        GPUVertexBufferLayoutDescriptor.t js_array Js.t optdef readonly_prop
    end

  type t = js Js.t

  let create ?indexFormat ?vertexBuffers () =
    object%js
      val indexFormat = Optdef.option indexFormat

      val vertexBuffers = Optdef.option vertexBuffers
    end
  ;;
end

module GPUPrimitiveTopology = struct
  type t = js_string Js.t
end

module GPUFrontFace = struct
  type t = js_string Js.t
end

module GPUCullMode = struct
  type t = js_string Js.t
end

module GPURasterizationStateDescriptor = struct
  class type js =
    object
      method frontFace : GPUFrontFace.t optdef readonly_prop

      method cullMode : GPUCullMode.t optdef readonly_prop

      method depthBias : i32 optdef readonly_prop

      method depthBiasSlopeScale : float optdef readonly_prop

      method depthBiasClamp : float optdef readonly_prop
    end

  type t = js Js.t

  let create ?frontFace ?cullMode ?depthBias ?depthBiasSlopeScale ?depthBiasClamp () =
    object%js
      val frontFace = Optdef.option frontFace

      val cullMode = Optdef.option cullMode

      val depthBias = Optdef.option depthBias

      val depthBiasSlopeScale = Optdef.option depthBiasSlopeScale

      val depthBiasClamp = Optdef.option depthBiasClamp
    end
  ;;
end

module GPURenderPipelineDescriptor = struct
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

  let create
      ?label
      ~layout
      ~vertexStage
      ?fragmentStage
      ~primitiveTopology
      ?rasterizationState
      ~colorStates
      ?depthStencilState
      ?vertexState
      ?sampleCount
      ?sampleMask
      ?alphaToCoverageEnabled
      ()
    =
    object%js
      val label = Optdef.option label

      val layout = layout

      val vertexStage = vertexStage

      val fragmentStage = Optdef.option fragmentStage

      val primitiveTopology = primitiveTopology

      val rasterizationState = Optdef.option rasterizationState

      val colorStates = colorStates

      val depthStencilState = Optdef.option depthStencilState

      val vertexState = Optdef.option vertexState

      val sampleCount = Optdef.option sampleCount

      val sampleMask = Optdef.option sampleMask

      val alphaToCoverageEnabled = Optdef.option alphaToCoverageEnabled
    end
  ;;
end

module GPURenderPipeline = struct
  type witness

  class type js =
    object
      method gpurenderpipeline_witness : witness

      inherit GPUObjectBase.js
    end

  type t = js Js.t
end

module GPUCommandEncoderDescriptor = struct
  class type js =
    object
      inherit GPUObjectDescriptorBase.js
    end

  type t = js Js.t

  let create ?label () =
    object%js
      val label = Optdef.option label
    end
  ;;
end

module GPULoadOp = struct
  type t = js_string Js.t
end

module GPUStoreOp = struct
  type t = js_string Js.t
end

module GPURenderPassColorAttachmentDescriptor = struct
  class type js =
    object
      method attachment : GPUTextureView.t readonly_prop

      method resolveTarget : GPUTextureView.t optdef readonly_prop

      method loadValue : (GPULoadOp.t, GPUColor.t) Union.t Js.t readonly_prop

      method storeOp : GPUStoreOp.t optdef readonly_prop
    end

  type t = js Js.t

  let create ~attachment ?resolveTarget ~loadValue ?storeOp () =
    object%js
      val attachment = attachment

      val resolveTarget = Optdef.option resolveTarget

      val loadValue = loadValue

      val storeOp = Optdef.option storeOp
    end
  ;;
end

module GPURenderPassDepthStencilAttachmentDescriptor = struct
  class type js =
    object
      method attachment : GPUTextureView.t readonly_prop

      method depthLoadValue : (GPULoadOp.t, float) Union.t Js.t readonly_prop

      method depthStoreOp : GPUStoreOp.t readonly_prop

      method stencilLoadValue : (GPULoadOp.t, u32) Union.t Js.t readonly_prop

      method stencilStoreOp : GPUStoreOp.t readonly_prop
    end

  type t = js Js.t

  let create
      ~attachment
      ~depthLoadValue
      ~depthStoreOp
      ~stencilLoadValue
      ~stencilStoreOp
      ()
    =
    object%js
      val attachment = attachment

      val depthLoadValue = depthLoadValue

      val depthStoreOp = depthStoreOp

      val stencilLoadValue = stencilLoadValue

      val stencilStoreOp = stencilStoreOp
    end
  ;;
end

module GPURenderPassDescriptor = struct
  class type js =
    object
      inherit GPUObjectDescriptorBase.js

      method colorAttachments :
        GPURenderPassColorAttachmentDescriptor.t js_array Js.t readonly_prop

      method depthStencilAttachment :
        GPURenderPassDepthStencilAttachmentDescriptor.t optdef readonly_prop
    end

  type t = js Js.t

  let create ?label ~colorAttachments ?depthStencilAttachment () =
    object%js
      val label = Optdef.option label

      val colorAttachments = colorAttachments

      val depthStencilAttachment = Optdef.option depthStencilAttachment
    end
  ;;
end

module GPUBufferCopyView = struct
  class type js =
    object
      method buffer : GPUBuffer.t readonly_prop

      method offset : u64 optdef readonly_prop

      method bytesPerRow : u32 readonly_prop

      method rowsPerImage : u32 optdef readonly_prop
    end

  type t = js Js.t

  let create ~buffer ?offset ~bytesPerRow ?rowsPerImage () =
    object%js
      val buffer = buffer

      val offset = Optdef.option offset

      val bytesPerRow = bytesPerRow

      val rowsPerImage = Optdef.option rowsPerImage
    end
  ;;
end

module GPUTextureCopyView = struct
  class type js =
    object
      method texture : GPUTexture.t readonly_prop

      method mipLevel : u32 optdef readonly_prop

      method arrayLayer : u32 optdef readonly_prop

      method origin : GPUOrigin3D.t optdef readonly_prop
    end

  type t = js Js.t

  let create ~texture ?mipLevel ?arrayLayer ?origin () =
    object%js
      val texture = texture

      val mipLevel = Optdef.option mipLevel

      val arrayLayer = Optdef.option arrayLayer

      val origin = Optdef.option origin
    end
  ;;
end

module GPUProgrammablePassEncoder = struct
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

module GPURenderEncoderBase = struct
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

module GPURenderPassEncoder = struct
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

module GPUComputePassDescriptor = struct
  class type js =
    object
      inherit GPUObjectDescriptorBase.js
    end

  type t = js Js.t

  let create ?label () =
    object%js
      val label = Optdef.option label
    end
  ;;
end

module GPUComputePassEncoder = struct
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

module GPUCommandBufferDescriptor = struct
  class type js =
    object
      inherit GPUObjectDescriptorBase.js
    end

  type t = js Js.t

  let create ?label () =
    object%js
      val label = Optdef.option label
    end
  ;;
end

module GPUCommandBuffer = struct
  type witness

  class type js =
    object
      method gpucommandbuffer_witness : witness

      inherit GPUObjectBase.js
    end

  type t = js Js.t
end

module GPUCommandEncoder = struct
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

module GPUQueue = struct
  type witness

  class type js =
    object
      method gpuqueue_witness : witness

      inherit GPUObjectBase.js

      method submit : buffers:GPUCommandBuffer.t js_array Js.t -> unit meth
    end

  type t = js Js.t
end

module GPUDevice = struct
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

module GPUExtensions = struct
  class type js =
    object
      method anisotropicFiltering : bool Js.t optdef readonly_prop
    end

  type t = js Js.t

  let create ?anisotropicFiltering () =
    object%js
      val anisotropicFiltering = Optdef.option anisotropicFiltering
    end
  ;;
end

module GPULimits = struct
  class type js =
    object
      method maxBindGroups : u32 optdef readonly_prop
    end

  type t = js Js.t

  let create ?maxBindGroups () =
    object%js
      val maxBindGroups = Optdef.option maxBindGroups
    end
  ;;
end

module GPUDeviceDescriptor = struct
  class type js =
    object
      method extensions : GPUExtensions.t optdef readonly_prop

      method limits : GPULimits.t optdef readonly_prop
    end

  type t = js Js.t

  let create ?extensions ?limits () =
    object%js
      val extensions = Optdef.option extensions

      val limits = Optdef.option limits
    end
  ;;
end

module GPUAdapter = struct
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

module GPU = struct
  type witness

  class type js =
    object
      method gpu_witness : witness

      method requestAdapter :
        options:GPURequestAdapterOptions.t optdef -> GPUAdapter.t Promise.t Js.t meth
    end

  type t = js Js.t
end

module GPUProvider = struct
  type witness

  class type js =
    object
      method gpuprovider_witness : witness

      method gpu : GPU.t readonly_prop
    end

  type t = js Js.t
end

module GPUDeviceLostInfo = struct
  type witness

  class type js =
    object
      method gpudevicelostinfo_witness : witness

      method message : js_string Js.t readonly_prop
    end

  type t = js Js.t
end

module GPUErrorFilter = struct
  type t = js_string Js.t
end

module GPUOutOfMemoryError = struct
  type witness

  class type js =
    object
      method gpuoutofmemoryerror_witness : witness
    end

  type t = js Js.t
end

module GPUValidationError = struct
  type witness

  class type js =
    object
      method gpuvalidationerror_witness : witness
    end

  type t = js Js.t
end

module GPUError = struct
  type t = (GPUOutOfMemoryError.t, GPUValidationError.t) Union.t Js.t
end

module GPUBufferUsage = struct
  type witness

  class type js =
    object
      method gpubufferusage_witness : witness
    end

  type t = js Js.t

  let _NONE = 0
  let _MAP_READ = 1
  let _MAP_WRITE = 2
  let _COPY_SRC = 4
  let _COPY_DST = 8
  let _INDEX = 16
  let _VERTEX = 32
  let _UNIFORM = 64
  let _STORAGE = 128
  let _INDIRECT = 256
end

module GPUTextureUsage = struct
  type witness

  class type js =
    object
      method gputextureusage_witness : witness
    end

  type t = js Js.t

  let _NONE = 0
  let _COPY_SRC = 1
  let _COPY_DST = 2
  let _SAMPLED = 4
  let _STORAGE = 8
  let _OUTPUT_ATTACHMENT = 16
end

module GPUShaderStage = struct
  type witness

  class type js =
    object
      method gpushaderstage_witness : witness
    end

  type t = js Js.t

  let _NONE = 0
  let _VERTEX = 1
  let _FRAGMENT = 2
  let _COMPUTE = 4
end

module GPUColorWrite = struct
  type witness

  class type js =
    object
      method gpucolorwrite_witness : witness
    end

  type t = js Js.t

  let _NONE = 0
  let _RED = 1
  let _GREEN = 2
  let _BLUE = 4
  let _ALPHA = 8
  let _ALL = 15
end

module GPUImageBitmapCopyView = struct
  class type js =
    object
      method origin : GPUOrigin2D.t optdef readonly_prop
    end

  type t = js Js.t

  let create ?origin () =
    object%js
      val origin = Optdef.option origin
    end
  ;;
end

module GPURenderBundleEncoderDescriptor = struct
  class type js =
    object
      inherit GPUObjectDescriptorBase.js

      method colorFormats : GPUTextureFormat.t js_array Js.t readonly_prop

      method depthStencilFormat : GPUTextureFormat.t optdef readonly_prop

      method sampleCount : u32 optdef readonly_prop
    end

  type t = js Js.t

  let create ?label ~colorFormats ?depthStencilFormat ?sampleCount () =
    object%js
      val label = Optdef.option label

      val colorFormats = colorFormats

      val depthStencilFormat = Optdef.option depthStencilFormat

      val sampleCount = Optdef.option sampleCount
    end
  ;;
end

module GPURenderBundleEncoder = struct
  type witness

  class type js =
    object
      method gpurenderbundleencoder_witness : witness

      inherit GPUObjectBase.js
    end

  type t = js Js.t
end

module GPURenderBundleDescriptor = struct
  class type js =
    object
      inherit GPUObjectDescriptorBase.js
    end

  type t = js Js.t

  let create ?label () =
    object%js
      val label = Optdef.option label
    end
  ;;
end

module GPURenderBundle = struct
  type witness

  class type js =
    object
      method gpurenderbundle_witness : witness

      inherit GPUObjectBase.js
    end

  type t = js Js.t
end

module GPUFenceDescriptor = struct
  class type js =
    object
      inherit GPUObjectDescriptorBase.js

      method initialValue : u64 optdef readonly_prop
    end

  type t = js Js.t

  let create ?label ?initialValue () =
    object%js
      val label = Optdef.option label

      val initialValue = Optdef.option initialValue
    end
  ;;
end

module GPUFence = struct
  type witness

  class type js =
    object
      method gpufence_witness : witness

      inherit GPUObjectBase.js
    end

  type t = js Js.t
end

module GPUSwapChain = struct
  type witness

  class type js =
    object
      method gpuswapchain_witness : witness

      inherit GPUObjectBase.js

      method getCurrentTexture : GPUTexture.t meth
    end

  type t = js Js.t
end

module GPUSwapChainDescriptor = struct
  class type js =
    object
      inherit GPUObjectDescriptorBase.js

      method device : GPUDevice.t readonly_prop

      method format : GPUTextureFormat.t readonly_prop

      method usage : GPUTextureUsageFlags.t optdef readonly_prop
    end

  type t = js Js.t

  let create ?label ~device ~format ?usage () =
    object%js
      val label = Optdef.option label

      val device = device

      val format = format

      val usage = Optdef.option usage
    end
  ;;
end

module GPUCanvasContext = struct
  type witness

  class type js =
    object
      method gpucanvascontext_witness : witness

      method configureSwapChain :
        descriptor:GPUSwapChainDescriptor.t -> GPUSwapChain.t meth
    end

  type t = js Js.t
end
