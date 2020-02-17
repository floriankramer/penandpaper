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
import Matrix from './matrix'

export enum ShaderInputType {
  POSITION,
  VERTEX_COLOR
}

const ShaderInputTypeSize = [2, 4]

export default class Actor {
  material: Material = new Material()

  isBuilt = false

  vertexShaderInput: Map<ShaderInputType, number[]> = new Map()
  vbos: Map<ShaderInputType, WebGLBuffer> = new Map()

  _scale: number[] = [1, 1]
  _position: number[] = [0, 0]

  _transform: Matrix = new Matrix()

  build (ctx: WebGLRenderingContext) {
    this.isBuilt = true
    for (const input of this.vertexShaderInput) {
      let vbo = ctx.createBuffer()
      if (!vbo) {
        continue
      }
      this.vbos.set(input[0], vbo)
      ctx.bindBuffer(ctx.ARRAY_BUFFER, vbo)
      ctx.bufferData(ctx.ARRAY_BUFFER, new Float32Array(input[1]), ctx.STATIC_DRAW)
    }
  }

  activate (ctx: WebGLRenderingContext) {
    if (!this.isBuilt) {
      this.build(ctx)
    }
    for (const input of this.vertexShaderInput) {
      let attrPos = this.material.attributes.get(input[0])
      let vbo = this.vbos.get(input[0])
      if (attrPos && vbo) {
        ctx.bindBuffer(ctx.ARRAY_BUFFER, vbo)
        ctx.vertexAttribPointer(
          attrPos,
          ShaderInputTypeSize[input[0].valueOf()],
          ctx.FLOAT,
          false,
          0,
          0)
        ctx.enableVertexAttribArray(attrPos)
      }
    }
  }

  callDraw (ctx: WebGLRenderingContext) {
    let positions = this.vertexShaderInput.get(ShaderInputType.POSITION)
    if (positions) {
      ctx.drawArrays(ctx.TRIANGLES, 0, positions.length / 2)
    }
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

  setVertexDataChanged () {
    this.isBuilt = false
  }
}
