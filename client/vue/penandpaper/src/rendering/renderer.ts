import Actor from "./actor";
import Camera from "./camera";
import DiffuseMaterial from "./diffuse_material";
import ShaderCache from "./shader_cache";

export default class Renderer {
  sceneTree: Actor[] = []
  camera: Camera = new Camera()

  ctx?: WebGLRenderingContext

  shaderCache: ShaderCache = new ShaderCache()

  init () {
    if (this.ctx === undefined) {
      return
    }
    this.ctx.clearColor(0, 0, 0, 1)
    this.ctx.disable(this.ctx.DEPTH_TEST)
    this.ctx.disable(this.ctx.CULL_FACE)
    this.camera.height = 10

    // Debug code
    let a = new Actor()
    a.material = new DiffuseMaterial()
    a.positions = [0, 0, 0, 1, 1, 1]
    this.sceneTree.push(a)
  }

  beginFrame () {
    if (this.ctx === undefined) {
      return
    }
    this.ctx.clear(WebGLRenderingContext.COLOR_BUFFER_BIT)
    this.camera.updateMatrix()
  }

  drawFrame () {
    if (this.ctx === undefined) {
      return
    }
    this.sceneTree.forEach(actor => {
      actor.material.activate(this.ctx, this.shaderCache, this.camera)
      actor.activate(this.ctx)
      actor.callDraw(this.ctx)
    });
  }

  endFrame () {
    if (this.ctx === undefined) {
      return
    }
  }

  addActor (a: Actor) {
    this.sceneTree.push(a)
  }

  removeActor (a: Actor) {
    let i = this.sceneTree.findIndex(e => { e == a})
    if (i >= 0) {
      this.sceneTree.splice(i, 1)
    }
  }
}