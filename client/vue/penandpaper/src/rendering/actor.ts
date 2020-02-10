import Material from './material'


export default class Actor {
  material: Material = new Material()

  isBuilt = false

  posBuffer?: WebGLBuffer = undefined

  positions: number[] = []

  build (ctx: WebGLRenderingContext) {
    this.isBuilt = true
    this.posBuffer = ctx.createBuffer()
    ctx.bindBuffer(ctx.ARRAY_BUFFER, this.posBuffer)
    ctx.bufferData(ctx.ARRAY_BUFFER, new Float32Array(this.positions), ctx.STATIC_DRAW)
    console.log('buffered the data: ', this.positions)
  }

  activate (ctx: WebGLRenderingContext) {
    if (!this.isBuilt) {
      this.build(ctx)
    }
    ctx.bindBuffer(ctx.ARRAY_BUFFER, this.posBuffer)
    console.log('posattr:', this.material.positionAttr)
    ctx.vertexAttribPointer(
      this.material.positionAttr,
      2,
      ctx.FLOAT,
      false,
      0,
      0);
    ctx.enableVertexAttribArray(this.material.positionAttr);
    console.log('activating the position arrays')
  }

  callDraw (ctx: WebGLRenderingContext) {
    console.log('drawing ', this.positions.length / 2, ' vertices as triangles')
    ctx.drawArrays(ctx.TRIANGLES, 0, this.positions.length / 2)
  }
}