import Material from './material'
import Camera from './camera'
import ShaderCache from './shader_cache'


export default class DiffuseMaterial extends Material {

  r: number = 1
  g: number = 1
  b: number = 1
  a: number = 1

  colorLoc: WebGLUniformLocation = 0

  constructor () {
    super()

    this.vertexShaderSrc = `
      attribute vec4 aPos;

      void main() {
        gl_Position = aPos;
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
    console.log('pos atttr', this.positionAttr)
  }

  activate (ctx: WebGLRenderingContext, shaderCache: ShaderCache, cam: Camera) {
    super.activate(ctx, shaderCache, cam)
    ctx.uniform4f(this.colorLoc, this.r, this.g, this.b, this.a)
  }
}