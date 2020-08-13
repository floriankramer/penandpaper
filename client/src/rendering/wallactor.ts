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
import { Wall } from '../simulation/building'

export default class WallActor extends Actor {
  walls: Wall[] = []

  showInvisible = false

  constructor () {
    super()
    this.material = new DiffuseMaterial()
    let dm = this.material as DiffuseMaterial
    dm.enableVertexColors()
    this.updateVertexData()
  }

  addWall (w: Wall) {
    this.walls.push(w)
    this.updateVertexData()
  }

  removeWall (w: Wall) {
    let idx = this.walls.findIndex((or) => w === or)
    if (idx >= 0) {
      this.walls.splice(idx, 1)
    }
    this.updateVertexData()
  }

  clearWalls () {
    this.walls = []
    this.updateVertexData()
  }

  updateVertexData () {
    let positions: number[] = []
    let colors: number[] = []

    const thickness = 0.17 / 2
    const outline = 0.05 / 2

    // The outline
    this.walls.forEach((w) => {
      if (!w.isVisible && !this.showInvisible) {
        return
      }
      let alpha = 1
      if (!w.isVisible) {
        alpha = 0.5
      }
      let dx = w.end.x - w.start.x
      let dy = w.end.y - w.start.y
      let l = Math.hypot(dx, dy)
      dx /= l
      dy /= l

      let nx = -dy
      let ny = dx

      let dxo = dx * outline
      let dyo = dy * outline
      let nxo = nx * outline
      let nyo = ny * outline

      positions.push(w.start.x + nx * thickness + nxo - dxo, w.start.y + ny * thickness + nyo - dyo)
      positions.push(w.start.x - nx * thickness - nxo - dxo, w.start.y - ny * thickness - nyo - dyo)
      positions.push(w.end.x + nx * thickness + nxo + dxo, w.end.y + ny * thickness + nyo + dyo)

      positions.push(w.start.x - nx * thickness - nxo - dxo, w.start.y - ny * thickness - nyo - dyo)
      positions.push(w.end.x - nx * thickness - nxo + dxo, w.end.y - ny * thickness - nyo + dyo)
      positions.push(w.end.x + nx * thickness + nxo + dxo, w.end.y + ny * thickness + nyo + dyo)

      colors.push(0.067, 0.067, 0.067, alpha)
      colors.push(0.067, 0.067, 0.067, alpha)
      colors.push(0.067, 0.067, 0.067, alpha)
      colors.push(0.067, 0.067, 0.067, alpha)
      colors.push(0.067, 0.067, 0.067, alpha)
      colors.push(0.067, 0.067, 0.067, alpha)
    })

    // The core
    this.walls.forEach((w) => {
      if (!w.isVisible && !this.showInvisible) {
        return
      }
      let alpha = 1
      if (!w.isVisible) {
        alpha = 0.7
      }

      let dx = w.end.x - w.start.x
      let dy = w.end.y - w.start.y
      let l = Math.hypot(dx, dy)
      dx /= l
      dy /= l

      let nx = -dy
      let ny = dx

      positions.push(w.start.x + nx * thickness, w.start.y + ny * thickness)
      positions.push(w.start.x - nx * thickness, w.start.y - ny * thickness)
      positions.push(w.end.x + nx * thickness, w.end.y + ny * thickness)

      positions.push(w.start.x - nx * thickness, w.start.y - ny * thickness)
      positions.push(w.end.x - nx * thickness, w.end.y - ny * thickness)
      positions.push(w.end.x + nx * thickness, w.end.y + ny * thickness)

      colors.push(0.133, 0.133, 0.133, alpha)
      colors.push(0.133, 0.133, 0.133, alpha)
      colors.push(0.133, 0.133, 0.133, alpha)
      colors.push(0.133, 0.133, 0.133, alpha)
      colors.push(0.133, 0.133, 0.133, alpha)
      colors.push(0.133, 0.133, 0.133, alpha)
    })

    this.vertexShaderInput.set(ShaderInputType.POSITION, positions)
    this.vertexShaderInput.set(ShaderInputType.VERTEX_COLOR, colors)
    this.setVertexDataChanged()
  }
}
