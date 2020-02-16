import Actor from "./actor";
import Camera from "./camera";
import DiffuseMaterial from "./diffuse_material";
import ShaderCache from "./shader_cache";

export default class Renderer {
  sceneTree: Actor[][] = []
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
    this.camera.reset()
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
    let ctx: WebGLRenderingContext = this.ctx
    this.sceneTree.forEach(actors => {
      actors.forEach(actor => {
        actor.material.activate(ctx, this.shaderCache, this.camera, actor)
        actor.activate(ctx)
        actor.callDraw(ctx)
      })
    });
  }

  endFrame () {
    if (this.ctx === undefined) {
      return
    }
  }

  addActor (a: Actor, layer: number) {
    if (layer >= this.sceneTree.length) {
      while (0 < layer + 1 - this.sceneTree.length) {
        this.sceneTree.push([])
      }
    }
    this.sceneTree[layer].push(a)
  }

  removeActor (a: Actor) {
    for (let layer of this.sceneTree) {
      let i = layer.findIndex(e => { return e === a })
      if (i >= 0) {
        layer.splice(i, 1)
      }
    }
  }

  onResize (width: number, height: number) {
    this.camera.aspectRatio = width / height
    this.camera.heightPixels = height
    if (this.ctx) {
      this.ctx.viewport(0, 0, width, height)
    }
  }
}