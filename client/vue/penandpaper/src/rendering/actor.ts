import Material from './material'
import Matrix from './matrix'


export default class Actor {
  material: Material = new Material()

  isBuilt = false

  posBuffer?: WebGLBuffer = undefined

  positions: number[] = []

  _scale: number[] = [1, 1]
  _position: number[] = [0, 0]

  _transform: Matrix = new Matrix()

  build (ctx: WebGLRenderingContext) {
    this.isBuilt = true
    this.posBuffer = ctx.createBuffer()
    ctx.bindBuffer(ctx.ARRAY_BUFFER, this.posBuffer)
    ctx.bufferData(ctx.ARRAY_BUFFER, new Float32Array(this.positions), ctx.STATIC_DRAW)
  }

  activate (ctx: WebGLRenderingContext) {
    if (!this.isBuilt) {
      this.build(ctx)
    }
    ctx.bindBuffer(ctx.ARRAY_BUFFER, this.posBuffer)
    ctx.vertexAttribPointer(
      this.material.positionAttr,
      2,
      ctx.FLOAT,
      false,
      0,
      0);
    ctx.enableVertexAttribArray(this.material.positionAttr);
  }

  callDraw (ctx: WebGLRenderingContext) {
    ctx.drawArrays(ctx.TRIANGLES, 0, this.positions.length / 2)
  }

  setPosition (x: number, y: number) {
    this._position[0] = x
    this._position[1] = y
    this._updateTransform()
  }

  translate (x: number, y: number) {
    this._position[0] += x
    this._position[1] += y
    this._updateTransform()
  }

  setScale (sx: number, sy: number) {
    this._scale[0] = sx
    this._scale[1] = sy
    this._updateTransform()
  }

  scale (sx: number, sy: number) {
    this._scale[0] += sx
    this._scale[1] += sy
    this._updateTransform()
  }
  
  _updateTransform () {
    this._transform.setIdentity()
    this._transform.scale(this._scale[0], this._scale[1])
    this._transform.translate(this._position[0], this._position[1])
  }

  rawTransform () {
    return this._transform.data
  }
}