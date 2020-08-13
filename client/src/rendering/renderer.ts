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
import { FontLoader } from './fontloader'

export default class Renderer {
  sceneTree: Actor[][] = []
  camera: Camera = new Camera()

  gl?: WebGLRenderingContext

  shaderCache: ShaderCache = new ShaderCache()

  init () {
    if (this.gl === undefined) {
      return
    }
    // Prepare the sdf font
    FontLoader.loadPrecomputedFont('res/font', this.gl)
    this.gl.clearColor(0, 0, 0, 1)
    this.gl.disable(this.gl.DEPTH_TEST)
    this.gl.disable(this.gl.CULL_FACE)
    this.gl.enable(this.gl.BLEND)
    this.gl.blendFunc(this.gl.SRC_ALPHA, this.gl.ONE_MINUS_SRC_ALPHA)
    this.camera.reset()
  }

  beginFrame () {
    if (this.gl === undefined) {
      return
    }
    this.gl.clear(WebGLRenderingContext.COLOR_BUFFER_BIT)
    this.camera.updateMatrix()
  }

  drawFrame () {
    if (this.gl === undefined) {
      return
    }
    let gl: WebGLRenderingContext = this.gl
    this.sceneTree.forEach(actors => {
      actors.forEach(actor => {
        actor.material.activate(gl, this.shaderCache, this.camera, actor)
        actor.activate(gl)
        actor.callDraw(gl)
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
    if (this.gl) {
      this.gl.viewport(0, 0, width, height)
    }
  }
}
