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

export default class TokenActor extends Actor {
  _r: number = 1
  _g: number = 1
  _b: number = 1

  sx: number = 0
  sy: number = 0
  ex: number = 0
  ey: number = 0
  thickness: number = 0

  constructor () {
    super()
    this.material = new DiffuseMaterial()
    this.setColor(this._r, this._g, this._b)
  }

  setLine (sx: number, sy: number, ex: number, ey: number, thickness: number) {
    this.sx = sx
    this.sy = sy
    this.ex = ex
    this.ey = ey
    this.thickness = thickness
    this.updateVertexData()
  }

  setColor (r: number, g: number, b: number) {
    this._r = r
    this._g = g
    this._b = b
    let dm = this.material as DiffuseMaterial
    dm.r = r
    dm.g = g
    dm.b = b
  }

  updateVertexData () {
    // A circle
    let positions = []

    let dx = this.ex - this.sx
    let dy = this.ey - this.sy
    let l = Math.hypot(dx, dy)
    dx /= l
    dy /= l

    let nx = -dy
    let ny = dx

    positions.push(this.sx + nx * this.thickness, this.sy + ny * this.thickness)
    positions.push(this.sx - nx * this.thickness, this.sy - ny * this.thickness)
    positions.push(this.ex + nx * this.thickness, this.ey + ny * this.thickness)

    positions.push(this.sx - nx * this.thickness, this.sy - ny * this.thickness)
    positions.push(this.ex - nx * this.thickness, this.ey - ny * this.thickness)
    positions.push(this.ex + nx * this.thickness, this.ey + ny * this.thickness)

    this.vertexShaderInput.set(ShaderInputType.POSITION, positions)
    this.setVertexDataChanged()
  }
}
