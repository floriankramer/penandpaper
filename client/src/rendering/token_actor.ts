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

  _isFoe: boolean = false
  _isSelected: boolean = false

  constructor () {
    super()
    this.material = new DiffuseMaterial()
    let dm = this.material as DiffuseMaterial
    dm.enableVertexColors()
    this.updateVertexData()
  }

  setColor (r: number, g: number, b: number) {
    this._r = r
    this._g = g
    this._b = b
    this.updateVertexData()
  }

  setIsFoe (isFoe: boolean) {
    this._isFoe = isFoe
    this.updateVertexData()
  }

  setIsSelected (isSelected: boolean) {
    this._isSelected = isSelected
    this.updateVertexData()
  }

  updateVertexData () {
    // A circle
    let sampleCount = 32
    let step = 2 * Math.PI / sampleCount
    let positions = []
    let colors = []
    for (let i = 0; i < sampleCount; ++i) {
      let a = i * step
      let a2 = (i + 1) * step
      positions.push(0, 0)
      positions.push(Math.cos(a), Math.sin(a))
      positions.push(Math.cos(a2), Math.sin(a2))
      if (i === 0 || i + 1 === sampleCount) {
        colors.push(1, 1, 1, 1)
        colors.push(1, 1, 1, 1)
        colors.push(1, 1, 1, 1)
      } else {
        colors.push(this._r, this._g, this._b, 1)
        colors.push(this._r, this._g, this._b, 1)
        colors.push(this._r, this._g, this._b, 1)
      }
    }

    if (this._isSelected || this._isFoe) {
      const ringSize = 1.15

      for (let i = 0; i < sampleCount; ++i) {
        let a = i * step
        let a2 = (i + 1) * step
        // first triangle
        positions.push(Math.cos(a) * ringSize, Math.sin(a) * ringSize)
        positions.push(Math.cos(a), Math.sin(a))
        positions.push(Math.cos(a2) * ringSize, Math.sin(a2) * ringSize)

        // second triangle
        positions.push(Math.cos(a2) * ringSize, Math.sin(a2) * ringSize)
        positions.push(Math.cos(a), Math.sin(a))
        positions.push(Math.cos(a2), Math.sin(a2))
      }
      if (this._isFoe && this._isSelected) {
        for (let i = 0; i < sampleCount * 6; ++i) {
          colors.push(1, 0.5, 0.5, 1)
        }
      } else if (this._isFoe) {
        for (let i = 0; i < sampleCount * 6; ++i) {
          colors.push(1, 0, 0, 1)
        }
      } else {
        for (let i = 0; i < sampleCount * 6; ++i) {
          colors.push(1, 1, 1, 1)
        }
      }
    }

    this.vertexShaderInput.set(ShaderInputType.POSITION, positions)
    this.vertexShaderInput.set(ShaderInputType.VERTEX_COLOR, colors)
    this.setVertexDataChanged()
  }
}
