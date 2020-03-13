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

import * as F from './fontloader'

export default class FontMaterial extends Material {
  r: number = 1
  g: number = 1
  b: number = 1
  a: number = 1

  colorLoc: WebGLUniformLocation | null = null
  camLoc: WebGLUniformLocation | null = null
  modelLoc: WebGLUniformLocation | null = null
  texLoc: WebGLUniformLocation | null = null

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

      uniform vec4 uColor;
      uniform sampler2D uFontTexture;

      varying vec2 vUV;

      void main() {
        vec4 val = texture2D(uFontTexture, vUV);
        float cutoff = step(0.75, val.r);
        gl_FragColor = uColor * cutoff;
      }
    `
  }

  build (ctx: WebGLRenderingContext, shaderCache: ShaderCache) {
    super.build(ctx, shaderCache)
    if (!this.program) {
      return
    }

    this.attributes.set(ShaderInputType.POSITION, ctx.getAttribLocation(this.program, 'aPos'))
    this.attributes.set(ShaderInputType.TEXTURE_COORD, ctx.getAttribLocation(this.program, 'aUV'))

    this.colorLoc = ctx.getUniformLocation(this.program, 'uColor')
    this.camLoc = ctx.getUniformLocation(this.program, 'uCamMat')
    this.modelLoc = ctx.getUniformLocation(this.program, 'uModelMat')
    this.texLoc = ctx.getUniformLocation(this.program, 'uFontTexture')
  }

  activate (gl: WebGLRenderingContext, shaderCache: ShaderCache, cam: Camera, actor: Actor) {
    super.activate(gl, shaderCache, cam, actor)
    gl.uniformMatrix4fv(this.camLoc, false, cam.getMatrix())
    gl.uniformMatrix4fv(this.modelLoc, false, actor.rawTransform())
    gl.uniform4f(this.colorLoc, this.r, this.g, this.b, this.a)

    gl.activeTexture(gl.TEXTURE0)
    gl.bindTexture(gl.TEXTURE_2D, F.FontLoader.default.texture)
    gl.uniform1i(this.texLoc, 0)
  }
}
