
// Provides: compile
async function compile(src)
{
  const glslangModule = await import(
    "https://unpkg.com/@webgpu/glslang@0.0.7/web/glslang.js"
  );
  
  const glslang = await glslangModule.default();
  
  return glslang.compileGLSL(src, "compute");
}

console.log("glslang");
