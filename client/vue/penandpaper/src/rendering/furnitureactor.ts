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
import { Furniture } from '../simulation/building'

export default class FurnitureActor extends Actor {
  furniture: Furniture[] = []

  constructor () {
    super()
    this.material = new DiffuseMaterial()
    let dm = this.material as DiffuseMaterial
    dm.enableVertexColors()
    this.updateVertexData()
  }

  addFurniture (w: Furniture) {
    this.furniture.push(w)
    this.updateVertexData()
  }

  removeFurniture (w: Furniture) {
    let idx = this.furniture.findIndex((or) => w === or)
    if (idx >= 0) {
      this.furniture.splice(idx, 1)
    }
    this.updateVertexData()
  }

  updateVertexData () {
    let positions: number[] = []
    let colors: number[] = []

    this.furniture.forEach((r) => {
      let srxo = Math.cos(r.rotation) * (r.size.x + 0.03)
      let sryo = -Math.sin(r.rotation) * (r.size.x + 0.03)

      let suxo = Math.sin(r.rotation) * (r.size.y + 0.03)
      let suyo = Math.cos(r.rotation) * (r.size.y + 0.03)

      // The outline
      positions.push(r.position.x - srxo - suxo, r.position.y - sryo - suyo)
      positions.push(r.position.x - srxo + suxo, r.position.y - sryo + suyo)
      positions.push(r.position.x + srxo - suxo, r.position.y + sryo - suyo)

      positions.push(r.position.x + srxo - suxo, r.position.y + sryo - suyo)
      positions.push(r.position.x - srxo + suxo, r.position.y - sryo + suyo)
      positions.push(r.position.x + srxo + suxo, r.position.y + sryo + suyo)

      colors.push(1, 1, 1, 1)
      colors.push(1, 1, 1, 1)
      colors.push(1, 1, 1, 1)

      colors.push(1, 1, 1, 1)
      colors.push(1, 1, 1, 1)
      colors.push(1, 1, 1, 1)

      let srx = Math.cos(r.rotation) * r.size.x
      let sry = -Math.sin(r.rotation) * r.size.x

      let sux = Math.sin(r.rotation) * r.size.y
      let suy = Math.cos(r.rotation) * r.size.y

      // The core
      positions.push(r.position.x - srx - sux, r.position.y - sry - suy)
      positions.push(r.position.x - srx + sux, r.position.y - sry + suy)
      positions.push(r.position.x + srx - sux, r.position.y + sry - suy)

      positions.push(r.position.x + srx - sux, r.position.y + sry - suy)
      positions.push(r.position.x - srx + sux, r.position.y - sry + suy)
      positions.push(r.position.x + srx + sux, r.position.y + sry + suy)

      colors.push(0.267, 0.267, 0.267, 1)
      colors.push(0.267, 0.267, 0.267, 1)
      colors.push(0.267, 0.267, 0.267, 1)

      colors.push(0.267, 0.267, 0.267, 1)
      colors.push(0.267, 0.267, 0.267, 1)
      colors.push(0.267, 0.267, 0.267, 1)
    })

    this.vertexShaderInput.set(ShaderInputType.POSITION, positions)
    this.vertexShaderInput.set(ShaderInputType.VERTEX_COLOR, colors)
    this.setVertexDataChanged()
  }
}
