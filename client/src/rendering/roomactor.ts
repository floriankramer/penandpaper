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
import { Room } from '../simulation/building'

export default class RoomActor extends Actor {
  rooms: Room[] = []

  showInvisible = false

  constructor () {
    super()
    this.material = new DiffuseMaterial()
    let dm = this.material as DiffuseMaterial
    dm.enableVertexColors()
    this.updateVertexData()
  }

  addRoom (r: Room) {
    this.rooms.push(r)
    this.updateVertexData()
  }

  removeRoom (r: Room) {
    let idx = this.rooms.findIndex((or) => r === or)
    if (idx >= 0) {
      this.rooms.splice(idx, 1)
    }
    this.updateVertexData()
  }

  clearRooms () {
    this.rooms = []
    this.updateVertexData()
  }

  updateVertexData () {
    let positions: number[] = []
    let colors: number[] = []

    this.rooms.forEach((r) => {
      if (!r.isVisible && !this.showInvisible) {
        return
      }
      let alpha = 1
      if (!r.isVisible) {
        alpha = 0.5
      }

      positions.push(r.position.x - r.size.x, r.position.y - r.size.y)
      positions.push(r.position.x - r.size.x, r.position.y + r.size.y)
      positions.push(r.position.x + r.size.x, r.position.y - r.size.y)

      positions.push(r.position.x + r.size.x, r.position.y - r.size.y)
      positions.push(r.position.x - r.size.x, r.position.y + r.size.y)
      positions.push(r.position.x + r.size.x, r.position.y + r.size.y)

      colors.push(0.27, 0.27, 0.27, alpha)
      colors.push(0.27, 0.27, 0.27, alpha)
      colors.push(0.27, 0.27, 0.27, alpha)

      colors.push(0.27, 0.27, 0.27, alpha)
      colors.push(0.27, 0.27, 0.27, alpha)
      colors.push(0.27, 0.27, 0.27, alpha)
    })

    this.vertexShaderInput.set(ShaderInputType.POSITION, positions)
    this.vertexShaderInput.set(ShaderInputType.VERTEX_COLOR, colors)
    this.setVertexDataChanged()
  }
}
