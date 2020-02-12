import Material from './material'
import Camera from './camera'
import ShaderCache from './shader_cache'
import Actor from './actor'


export default class DiffuseMaterial extends Material {

  r: number = 1
  g: number = 1
  b: number = 1
  a: number = 1

  colorLoc: WebGLUniformLocation = 0
  camLoc: WebGLUniformLocation = 0
  modelLoc: WebGLUniformLocation = 0

  constructor () {
    super()

    this.vertexShaderSrc = `
      attribute vec4 aPos;

      uniform mat4 uCamMat;
      uniform mat4 uModelMat;

      void main() {
        gl_Position = uCamMat * uModelMat * aPos;
      }
    `

    this.fragmentShaderSrc = `
      uniform highp vec4 uColor;

      void main() {
        gl_FragColor = uColor;
      }
    `
  }

  build (ctx: WebGLRenderingContext, shaderCache: ShaderCache) {
    super.build(ctx,shaderCache)
    this.positionAttr = ctx.getAttribLocation(this.program, 'aPos')
    this.colorLoc = ctx.getUniformLocation(this.program, 'uColor')
    this.camLoc = ctx.getUniformLocation(this.program, 'uCamMat')
    this.modelLoc = ctx.getUniformLocation(this.program, 'uModelMat')
  }

  activate (ctx: WebGLRenderingContext, shaderCache: ShaderCache, cam: Camera, actor: Actor) {
    super.activate(ctx, shaderCache, cam, actor)
    ctx.uniformMatrix4fv(this.camLoc, false, cam.getMatrix())
    ctx.uniformMatrix4fv(this.modelLoc, false, actor.rawTransform())
    ctx.uniform4f(this.colorLoc, this.r, this.g, this.b, this.a)
  }
}