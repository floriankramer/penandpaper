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
import { Door } from '../simulation/building'
import { Matrix3 } from './matrix'

export default class DoorActor extends Actor {
  doors: Door[] = []

  constructor () {
    super()
    this.material = new DiffuseMaterial()
    let dm = this.material as DiffuseMaterial
    dm.enableVertexColors()
    this.updateVertexData()
  }

  addDoor (w: Door) {
    this.doors.push(w)
    this.updateVertexData()
  }

  removeDoor (w: Door) {
    let idx = this.doors.findIndex((or) => w === or)
    if (idx >= 0) {
      this.doors.splice(idx, 1)
    }
    this.updateVertexData()
  }

  updateVertexData () {
    const closedColor = 0.4
    let positions: number[] = []
    let colors: number[] = []

    // Build a base door

    // door frame
    // thickness
    let t = 0.03
    let base: number[][] = []
    base.push([-t, 0.5])
    base.push([-t, -0.5])
    base.push([t, -0.5])

    base.push([t, -0.5])
    base.push([t, 0.5])
    base.push([-t, 0.5])

    // the door
    base.push([0, -0.5 - t])
    base.push([0, -0.5 + t])
    base.push([1, -0.5 + t])

    base.push([1, -0.5 + t])
    base.push([1, -0.5 - t])
    base.push([0, -0.5 - t])

    // The arc
    let steps = 8
    let astep = Math.PI / 2 / steps

    let a = 0
    let ls = 1 - t
    let ll = 1 + t
    for (let i = 0; i < steps; ++i) {
      let na = a + astep

      let aco = Math.cos(a)
      let asi = Math.sin(a)

      let naco = Math.cos(na)
      let nasi = Math.sin(na)

      base.push([aco * ls, asi * ls - 0.5])
      base.push([aco * ll, asi * ll - 0.5])
      base.push([naco * ll, nasi * ll - 0.5])

      base.push([naco * ll, nasi * ll - 0.5])
      base.push([naco * ls, nasi * ls - 0.5])
      base.push([aco * ls, asi * ls - 0.5])

      a += astep
    }

    this.doors.forEach((d) => {
      let m = new Matrix3()
      m.scale(d.width, d.width)
      m.rotate(d.rotation)
      m.translate(d.position.x, d.position.y)
      base.forEach((p: number[]) => {
        let p0 = m.mulVec(p[0], p[1], 1)
        console.log(p, p0)
        positions.push(p0[0], p0[1])
      })

      // Colors
      // The frame
      if (d.isOpen) {
        colors.push(closedColor, closedColor, closedColor, 1)
        colors.push(closedColor, closedColor, closedColor, 1)
        colors.push(closedColor, closedColor, closedColor, 1)

        colors.push(closedColor, closedColor, closedColor, 1)
        colors.push(closedColor, closedColor, closedColor, 1)
        colors.push(closedColor, closedColor, closedColor, 1)
      } else {
        colors.push(1, 1, 1, 1)
        colors.push(1, 1, 1, 1)
        colors.push(1, 1, 1, 1)

        colors.push(1, 1, 1, 1)
        colors.push(1, 1, 1, 1)
        colors.push(1, 1, 1, 1)
      }

      // The door
      if (d.isOpen) {
        colors.push(1, 1, 1, 1)
        colors.push(1, 1, 1, 1)
        colors.push(1, 1, 1, 1)

        colors.push(1, 1, 1, 1)
        colors.push(1, 1, 1, 1)
        colors.push(1, 1, 1, 1)
      } else {
        colors.push(closedColor, closedColor, closedColor, 1)
        colors.push(closedColor, closedColor, closedColor, 1)
        colors.push(closedColor, closedColor, closedColor, 1)

        colors.push(closedColor, closedColor, closedColor, 1)
        colors.push(closedColor, closedColor, closedColor, 1)
        colors.push(closedColor, closedColor, closedColor, 1)
      }

      // The Arc

      if (d.isOpen) {
        for (let i = 0; i < steps; ++i) {
          colors.push(1, 1, 1, 1)
          colors.push(1, 1, 1, 1)
          colors.push(1, 1, 1, 1)

          colors.push(1, 1, 1, 1)
          colors.push(1, 1, 1, 1)
          colors.push(1, 1, 1, 1)
        }
      } else {
        for (let i = 0; i < steps; ++i) {
          colors.push(closedColor, closedColor, closedColor, 1)
          colors.push(closedColor, closedColor, closedColor, 1)
          colors.push(closedColor, closedColor, closedColor, 1)

          colors.push(closedColor, closedColor, closedColor, 1)
          colors.push(closedColor, closedColor, closedColor, 1)
          colors.push(closedColor, closedColor, closedColor, 1)
        }
      }
    })

    console.log(positions)
    console.log(colors)

    this.vertexShaderInput.set(ShaderInputType.POSITION, positions)
    this.vertexShaderInput.set(ShaderInputType.VERTEX_COLOR, colors)
    this.setVertexDataChanged()
  }
}
