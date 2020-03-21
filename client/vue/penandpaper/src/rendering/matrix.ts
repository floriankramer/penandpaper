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

export default class Matrix {
  data: Float32Array = new Float32Array(16)

  constructor () {
    this.setIdentity()
  }

  setIdentity () {
    for (let x = 0; x < 4; x++) {
      for (let y = 0; y < 4; y++) {
        this.set(x, y, x === y ? 1 : 0)
      }
    }
  }

  at (row: number, col: number) : number {
    return this.data[row + col * 4]
  }

  set (row: number, col: number, val: number) {
    this.data[row + col * 4] = val
  }

  /**
   * @return The result of (this) * (other)
   */
  mul (other: Matrix) : Matrix {
    let m = new Matrix()
    for (let row = 0; row < 4; row++) {
      for (let col = 0; col < 4; col++) {
        let val = 0
        for (let i = 0; i < 4; ++i) {
          val += this.at(row, i) * other.at(i, col)
        }
        m.set(row, col, val)
      }
    }
    return m
  }

  translate (x: number = 0, y: number = 0, z: number = 0) {
    let m = new Matrix()
    m.set(0, 3, x)
    m.set(1, 3, y)
    m.set(2, 3, z)
    this.data = m.mul(this).data
  }

  scale (x: number = 1, y: number = 1, z: number = 1) {
    let m = new Matrix()
    m.set(0, 0, x)
    m.set(1, 1, y)
    m.set(2, 2, z)
    this.data = m.mul(this).data
  }

  rotate (r: number = 0) {
    let rcos = Math.cos(r)
    let rsin = Math.sin(r)
    let m = new Matrix()
    m.set(0, 0, rcos)
    m.set(0, 1, -rsin)
    m.set(1, 1, rcos)
    m.set(1, 0, rsin)
    this.data = m.mul(this).data
  }
}


export class Matrix3 {
  data: Float32Array = new Float32Array(9)

  constructor () {
    this.setIdentity()
  }

  setIdentity () {
    for (let x = 0; x < 3; x++) {
      for (let y = 0; y < 3; y++) {
        this.set(x, y, x === y ? 1 : 0)
      }
    }
  }

  at (row: number, col: number) : number {
    return this.data[row + col * 3]
  }

  set (row: number, col: number, val: number) {
    this.data[row + col * 3] = val
  }

  /**
   * @return The result of (this) * (other)
   */
  mul (other: Matrix) : Matrix {
    let m = new Matrix3()
    for (let row = 0; row < 3; row++) {
      for (let col = 0; col < 3; col++) {
        let val = 0
        for (let i = 0; i < 3; ++i) {
          val += this.at(row, i) * other.at(i, col)
        }
        m.set(row, col, val)
      }
    }
    return m
  }

  mulVec (x: number, y: number, z: number) : number[] {
    return [this.at(0, 0) * x + this.at(0, 1) * y + this.at(0, 2) * z,
      this.at(1, 0) * x + this.at(1, 1) * y + this.at(1, 2) * z,
      this.at(2, 0) * x + this.at(2, 1) * y + this.at(2, 2) * z]
  }

  translate (x: number = 0, y: number = 0) {
    let m = new Matrix3()
    m.set(0, 2, x)
    m.set(1, 2, y)
    this.data = m.mul(this).data
  }

  scale (x: number = 1, y: number = 1) {
    let m = new Matrix3()
    m.set(0, 0, x)
    m.set(1, 1, y)
    this.data = m.mul(this).data
  }

  rotate (r: number = 0) {
    let rcos = Math.cos(r)
    let rsin = Math.sin(r)
    let m = new Matrix3()
    m.set(0, 0, rcos)
    m.set(0, 1, -rsin)
    m.set(1, 1, rcos)
    m.set(1, 0, rsin)
    this.data = m.mul(this).data
  }
}
