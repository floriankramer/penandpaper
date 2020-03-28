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

import Actor, { ShaderInputType } from './actor'
import TexturedMaterial from './textured_material'

export default class GridActor extends Actor {
  textureDirty: boolean = false
  texturePath: string = ''

  texture: WebGLTexture | null = null

  renderCallback: null | (() => any) = null

  constructor () {
    super()
    this.material = new TexturedMaterial()

    let positions = [-1, -1, -1, 1, 1, -1, -1, 1, 1, -1, 1, 1]
    let uv = [0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 1]
    this.vertexShaderInput.set(ShaderInputType.POSITION, positions)
    this.vertexShaderInput.set(ShaderInputType.TEXTURE_COORD, uv)
  }

  callDraw (gl: WebGLRenderingContext) {
    if (this.texture !== null) {
      super.callDraw(gl)
    }
  }

  activate (gl: WebGLRenderingContext) {
    if (this.textureDirty) {
      this._loadTexture(gl)
    }
    super.activate(gl)
  }

  setTexturePath (path: string) {
    this.texturePath = path
    this.textureDirty = true
  }

  setRenderCallback (callback: (() => any) | null) {
    this.renderCallback = callback
  }

  _loadTexture (gl: WebGLRenderingContext) {
    this.textureDirty = false

    this.texture = gl.createTexture()
    if (this.texture === null) {
      console.log('Error: Unable to create a texture.')
      return
    }
    let tm = this.material as TexturedMaterial
    tm.setTexture(this.texture)

    gl.bindTexture(gl.TEXTURE_2D, this.texture)
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, 1, 1,
      0, gl.RGBA, gl.UNSIGNED_BYTE, new Uint8Array(4))
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR)
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR)
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE)
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE)

    // Load the actual texture
    const img = new Image()
    img.onload = () => {
      gl.bindTexture(gl.TEXTURE_2D, this.texture)
      gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA,
        gl.RGBA, gl.UNSIGNED_BYTE, img)
      if (this.renderCallback !== null) {
        this.renderCallback()
      }
    }
    img.onerror = () => {
      console.log('unable to load an image from ', img.src)
    }
    img.src = this.texturePath
  }
}
