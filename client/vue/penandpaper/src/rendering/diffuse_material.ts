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

export default class DiffuseMaterial extends Material {
  r: number = 1
  g: number = 1
  b: number = 1
  a: number = 1

  colorLoc: WebGLUniformLocation | null = null
  camLoc: WebGLUniformLocation | null = null
  modelLoc: WebGLUniformLocation | null = null

  _useVertexColors: boolean = false

  constructor () {
    super()

    this.vertexShaderSrc = `
      attribute vec4 aPos;

      uniform mat4 uCamMat;
      uniform mat4 uModelMat;

      void main() {
        gl_Position = uCamMat * uModelMat * aPos;
      }
    `

    this.fragmentShaderSrc = `
      precision highp float;

      uniform vec4 uColor;

      void main() {
        gl_FragColor = uColor;
      }
    `
  }

  build (ctx: WebGLRenderingContext, shaderCache: ShaderCache) {
    super.build(ctx, shaderCache)
    if (!this.program) {
      return
    }

    this.attributes.set(ShaderInputType.POSITION, ctx.getAttribLocation(this.program, 'aPos'))
    if (this._useVertexColors) {
      this.attributes.set(ShaderInputType.VERTEX_COLOR, ctx.getAttribLocation(this.program, 'aColor'))
    }

    this.colorLoc = ctx.getUniformLocation(this.program, 'uColor')
    this.camLoc = ctx.getUniformLocation(this.program, 'uCamMat')
    this.modelLoc = ctx.getUniformLocation(this.program, 'uModelMat')
  }

  activate (ctx: WebGLRenderingContext, shaderCache: ShaderCache, cam: Camera, actor: Actor) {
    super.activate(ctx, shaderCache, cam, actor)
    ctx.uniformMatrix4fv(this.camLoc, false, cam.getMatrix())
    ctx.uniformMatrix4fv(this.modelLoc, false, actor.rawTransform())
    ctx.uniform4f(this.colorLoc, this.r, this.g, this.b, this.a)
  }

  enableVertexColors () {
    this._useVertexColors = true
    this.vertexShaderSrc = `
      attribute vec4 aPos;
      attribute vec4 aColor;

      uniform mat4 uCamMat;
      uniform mat4 uModelMat;

      varying vec4 vColor;

      void main() {
        gl_Position = uCamMat * uModelMat * aPos;
        vColor = aColor;
      }
    `

    this.fragmentShaderSrc = `
      precision highp float;

      uniform vec4 uColor;
      varying vec4 vColor;

      void main() {
        gl_FragColor = uColor * vColor;
      }
    `
  }
}
