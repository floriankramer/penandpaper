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
import DiffuseMaterial from './diffuse_material'

export default class PingActor extends Actor {
  _r: number = 0.27
  _g: number = 0.55
  _b: number = 0.79

  constructor () {
    super()
    this.renderPrimitive = WebGLRenderingContext.LINES
    this.material = new DiffuseMaterial()
    let dm = this.material as DiffuseMaterial
    dm.r = this._r
    dm.g = this._g
    dm.b = this._b
    this.updateVertexData()
  }

  setColor (r: number, g: number, b: number) {
    this._r = r
    this._g = g
    this._b = b
    let dm = this.material as DiffuseMaterial
    dm.r = this._r
    dm.g = this._g
    dm.b = this._b
  }

  updateVertexData () {
    // A circle
    let sampleCount = 32
    let step = 2 * Math.PI / sampleCount
    let positions = []
    for (let i = 0; i < sampleCount; ++i) {
      let a = i * step
      let an = (i + 1) * step
      positions.push(Math.cos(a), Math.sin(a))
      positions.push(Math.cos(an), Math.sin(an))
    }
    this.vertexShaderInput.set(ShaderInputType.POSITION, positions)
    this.setVertexDataChanged()
  }
}
