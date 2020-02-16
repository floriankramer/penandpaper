import Camera from "./camera"
import ShaderCache from "./shader_cache"
import Actor, { ShaderInputType } from './actor'

export default class Material {
  vertexShaderSrc: string = ''
  fragmentShaderSrc: string = ''

  program: WebGLProgram | null = null

  isBuilt: boolean = false

  attributes: Map<ShaderInputType, number> = new Map()

  build (ctx: WebGLRenderingContext, shaderCache: ShaderCache) {
    // TODO: cache the program and reuse it where possible
    this.isBuilt = true
    let cached = shaderCache.getProgram(this.vertexShaderSrc, this.fragmentShaderSrc)
    if (cached !== undefined) {
      this.program = cached
    } else {
      let vertexShader = this.buildShader(ctx, this.vertexShaderSrc, ctx.VERTEX_SHADER)
      let fragmentShader = this.buildShader(ctx, this.fragmentShaderSrc, ctx.FRAGMENT_SHADER)

      if (vertexShader && fragmentShader) {
        this.program = ctx.createProgram()
        if (this.program) {
          ctx.attachShader(this.program, vertexShader)
          ctx.attachShader(this.program, fragmentShader)
          ctx.linkProgram(this.program)

          if (!ctx.getProgramParameter(this.program, ctx.LINK_STATUS)) {
            console.log('Error: unable to link the shader: ', ctx.getProgramInfoLog(this.program))
          }
          shaderCache.addProgram(this.vertexShaderSrc, this.fragmentShaderSrc, this.program)
        }
      }
    }
  }

  activate (ctx: WebGLRenderingContext, shaderCache: ShaderCache, cam: Camera, actor: Actor) {
    if (!this.isBuilt) {
      this.build(ctx, shaderCache)
    }
    if (this.program) {
      ctx.useProgram(this.program)
    }
  }

  buildShader (ctx: WebGLRenderingContext, src: string, type: number) : WebGLShader | null {
    let shader = ctx.createShader(type)
    if (shader) {
      ctx.shaderSource(shader, src)
      ctx.compileShader(shader)
      if (!ctx.getShaderParameter(shader, ctx.COMPILE_STATUS)) {
        console.log('Error: unable to compile the shader: ', src, ctx.getShaderInfoLog(shader))
        ctx.deleteShader(shader)
      }
    }
    return shader
  }
}
