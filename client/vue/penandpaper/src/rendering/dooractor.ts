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

  showInvisible = false

  arcSteps: number = 8

  constructor () {
    super()
    this.material = new DiffuseMaterial()
    let dm = this.material as DiffuseMaterial
    dm.enableVertexColors()
    this.updateVertexData()
  }

  clearDoors () {
    this.doors = []
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

  _buildDoor (scale: number): number[][] {
    // door frame
    // thickness
    let t = 0.03
    let baseT = 0.22 / 2
    let base: number[][] = []
    base.push([-baseT, 0.5 * scale])
    base.push([-baseT, -0.5 * scale])
    base.push([baseT, -0.5 * scale])

    base.push([baseT, -0.5 * scale])
    base.push([baseT, 0.5 * scale])
    base.push([-baseT, 0.5 * scale])

    // the door
    base.push([0, -0.5 * scale - t])
    base.push([0, -0.5 * scale + t])
    base.push([1 * scale, -0.5 * scale + t])

    base.push([1 * scale, -0.5 * scale + t])
    base.push([1 * scale, -0.5 * scale - t])
    base.push([0, -0.5 * scale - t])

    // The arc
    let astep = Math.PI / 2 / this.arcSteps

    let a = 0
    let ls = 1 * scale - t
    let ll = 1 * scale + t
    for (let i = 0; i < this.arcSteps; ++i) {
      let na = a + astep

      let aco = Math.cos(a)
      let asi = Math.sin(a)

      let naco = Math.cos(na)
      let nasi = Math.sin(na)

      base.push([aco * ls, asi * ls - 0.5 * scale])
      base.push([aco * ll, asi * ll - 0.5 * scale])
      base.push([naco * ll, nasi * ll - 0.5 * scale])

      base.push([naco * ll, nasi * ll - 0.5 * scale])
      base.push([naco * ls, nasi * ls - 0.5 * scale])
      base.push([aco * ls, asi * ls - 0.5 * scale])

      a += astep
    }
    return base
  }

  updateVertexData () {
    const closedColor = 0.27
    let positions: number[] = []
    let colors: number[] = []

    // Build a base door

    // door frame
    // thickness
    // let t = 0.03
    // let base: number[][] = []
    // base.push([-t, 0.5])
    // base.push([-t, -0.5])
    // base.push([t, -0.5])

    // base.push([t, -0.5])
    // base.push([t, 0.5])
    // base.push([-t, 0.5])

    // // the door
    // base.push([0, -0.5 - t])
    // base.push([0, -0.5 + t])
    // base.push([1, -0.5 + t])

    // base.push([1, -0.5 + t])
    // base.push([1, -0.5 - t])
    // base.push([0, -0.5 - t])

    // // The arc
    // let steps = 8
    // let astep = Math.PI / 2 / steps

    // let a = 0
    // let ls = 1 - t
    // let ll = 1 + t
    // for (let i = 0; i < steps; ++i) {
    //   let na = a + astep

    //   let aco = Math.cos(a)
    //   let asi = Math.sin(a)

    //   let naco = Math.cos(na)
    //   let nasi = Math.sin(na)

    //   base.push([aco * ls, asi * ls - 0.5])
    //   base.push([aco * ll, asi * ll - 0.5])
    //   base.push([naco * ll, nasi * ll - 0.5])

    //   base.push([naco * ll, nasi * ll - 0.5])
    //   base.push([naco * ls, nasi * ls - 0.5])
    //   base.push([aco * ls, asi * ls - 0.5])

    //   a += astep
    // }

    // the number of steps in the door arc
    this.doors.forEach((d) => {
      if (!d.isVisible && !this.showInvisible) {
        return
      }
      let alpha = 1
      if (!d.isVisible) {
        alpha = 0.5
      }

      let base = this._buildDoor(d.width)
      let m = new Matrix3()
      m.rotate(d.rotation)
      m.translate(d.position.x, d.position.y)
      base.forEach((p: number[]) => {
        let p0 = m.mulVec(p[0], p[1], 1)
        positions.push(p0[0], p0[1])
      })

      // Colors
      // The frame
      if (d.isOpen) {
        colors.push(closedColor, closedColor, closedColor, alpha)
        colors.push(closedColor, closedColor, closedColor, alpha)
        colors.push(closedColor, closedColor, closedColor, alpha)

        colors.push(closedColor, closedColor, closedColor, alpha)
        colors.push(closedColor, closedColor, closedColor, alpha)
        colors.push(closedColor, closedColor, closedColor, alpha)
      } else {
        colors.push(1, 1, 1, alpha)
        colors.push(1, 1, 1, alpha)
        colors.push(1, 1, 1, alpha)

        colors.push(1, 1, 1, alpha)
        colors.push(1, 1, 1, alpha)
        colors.push(1, 1, 1, alpha)
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
        colors.push(closedColor, closedColor, closedColor, alpha)
        colors.push(closedColor, closedColor, closedColor, alpha)
        colors.push(closedColor, closedColor, closedColor, alpha)

        colors.push(closedColor, closedColor, closedColor, alpha)
        colors.push(closedColor, closedColor, closedColor, alpha)
        colors.push(closedColor, closedColor, closedColor, alpha)
      }

      // The Arc

      if (d.isOpen) {
        for (let i = 0; i < this.arcSteps; ++i) {
          colors.push(1, 1, 1, alpha)
          colors.push(1, 1, 1, alpha)
          colors.push(1, 1, 1, alpha)

          colors.push(1, 1, 1, alpha)
          colors.push(1, 1, 1, alpha)
          colors.push(1, 1, 1, alpha)
        }
      } else {
        for (let i = 0; i < this.arcSteps; ++i) {
          colors.push(closedColor, closedColor, closedColor, alpha)
          colors.push(closedColor, closedColor, closedColor, alpha)
          colors.push(closedColor, closedColor, closedColor, alpha)

          colors.push(closedColor, closedColor, closedColor, alpha)
          colors.push(closedColor, closedColor, closedColor, alpha)
          colors.push(closedColor, closedColor, closedColor, alpha)
        }
      }
    })

    this.vertexShaderInput.set(ShaderInputType.POSITION, positions)
    this.vertexShaderInput.set(ShaderInputType.VERTEX_COLOR, colors)
    this.setVertexDataChanged()
  }
}
