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

import Matrix from './matrix'
import * as Sim from '../simulation/simulation'

export default class Camera {
  x: number = 0
  y: number = 0
  height: number = 10.01
  aspectRatio: number = 16 / 9

  heightPixels: number = 1

  // A 3x3 matrix
  matrix: Matrix = new Matrix()

  getMatrix () : Float32Array {
    return this.matrix.data
  }

  updateMatrix () {
    this.matrix.setIdentity()
    this.matrix.translate(-this.x, -this.y)
    this.matrix.scale(1 / this.height / this.aspectRatio, 1 / this.height)
  }

  zoom (dz: number) {
    if (dz < 0) {
      this.height *= 0.85
    } else if (dz > 0) {
      this.height *= 1.15
    }
    this.updateMatrix()
  }

  pan (dx: number, dy: number) {
    this.x -= dx * this.height * this.aspectRatio
    this.y -= dy * this.height
    this.updateMatrix()
  }

  reset () {
    this.height = 10.01
    this.x = 0
    this.y = 0
    this.updateMatrix()
  }

  screenToWorldSpace (sp: Sim.Point) : Sim.Point {
    return new Sim.Point((sp.x / this.heightPixels * 2 - this.aspectRatio) * this.height + this.x, -(sp.y / this.heightPixels * 2 - 1) * this.height + this.y)
  }

  worldToScreenSpace (sp: Sim.Point) : Sim.Point {
    return new Sim.Point(((sp.x - this.x) / this.height + this.aspectRatio) / 2 * this.heightPixels, -((sp.y - this.y) / this.height + 1) / 2 * this.heightPixels)
  }

  screenToWorldSpaceDist (d: number) : number {
    return d / this.heightPixels * 2 * this.height
  }

  worldToScreenSpaceDist (d: number) : number {
    return d / this.height / 2 * this.heightPixels
  }
}
