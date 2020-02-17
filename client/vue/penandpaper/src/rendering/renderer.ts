/**
 * Copyright 2020 Florian Kramer
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import Actor from './actor'
import Camera from './camera'
import DiffuseMaterial from './diffuse_material'
import ShaderCache from './shader_cache'

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
    })
  }

  endFrame () {
  }

  addActor (a: Actor, layer: number) {
    if (layer >= this.sceneTree.length) {
      while (layer + 1 - this.sceneTree.length > 0) {
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
