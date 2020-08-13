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

import Material from './material'
import Camera from './camera'
import ShaderCache from './shader_cache'
import Actor, { ShaderInputType } from './actor'

export default class TexturedMaterial extends Material {
  texLoc: WebGLUniformLocation | null = null
  camLoc: WebGLUniformLocation | null = null
  modelLoc: WebGLUniformLocation | null = null

  texture: WebGLTexture | null = null

  constructor () {
    super()

    this.vertexShaderSrc = `
      #version 100
      precision highp float;
      attribute vec4 aPos;
      attribute vec2 aUV;

      uniform mat4 uCamMat;
      uniform mat4 uModelMat;

      varying vec2 vUV;

      void main() {
        gl_Position = uCamMat * uModelMat * aPos;
        vUV = aUV;
      }
    `

    this.fragmentShaderSrc = `
      #version 100
      precision highp float;

      uniform sampler2D uTexture;

      varying vec2 vUV;

      void main() {
        gl_FragColor = texture2D(uTexture, vUV);
      }
    `
  }

  setTexture (texture: WebGLTexture) {
    this.texture = texture
  }

  build (ctx: WebGLRenderingContext, shaderCache: ShaderCache) {
    super.build(ctx, shaderCache)
    if (!this.program) {
      return
    }

    this.attributes.set(ShaderInputType.POSITION, ctx.getAttribLocation(this.program, 'aPos'))
    this.attributes.set(ShaderInputType.TEXTURE_COORD, ctx.getAttribLocation(this.program, 'aUV'))

    this.camLoc = ctx.getUniformLocation(this.program, 'uCamMat')
    this.modelLoc = ctx.getUniformLocation(this.program, 'uModelMat')
    this.texLoc = ctx.getUniformLocation(this.program, 'uTexture')
  }

  activate (gl: WebGLRenderingContext, shaderCache: ShaderCache, cam: Camera, actor: Actor) {
    super.activate(gl, shaderCache, cam, actor)
    gl.uniformMatrix4fv(this.camLoc, false, cam.getMatrix())
    gl.uniformMatrix4fv(this.modelLoc, false, actor.rawTransform())

    gl.activeTexture(gl.TEXTURE0)
    if (this.texture !== null) {
      gl.bindTexture(gl.TEXTURE_2D, this.texture)
    }
    gl.uniform1i(this.texLoc, 0)
  }
}
