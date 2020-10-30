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

export class Point {
  x: number = 0;
  y: number = 0;

  constructor (x: number, y: number) {
    this.x = x
    this.y = y
  }

  length () : number {
    return Math.hypot(this.x, this.y)
  }

  normalized () : Point {
    let l = this.length()
    return new Point(this.x / l, this.y / l)
  }

  normalize () {
    let l = this.length()
    this.x /= l
    this.y /= l
  }

  dot (other: Point) : number {
    return this.x * other.x + this.y * other.y
  }

  distTo (other: Point) : number {
    return Math.hypot(this.x - other.x, this.y - other.y)
  }

  minus (other: Point): Point {
    return new Point(this.x - other.x, this.y - other.y)
  }

  add (other: Point): Point {
    return new Point(this.x + other.x, this.y + other.y)
  }

  scale (scalar: number): Point {
    return new Point(this.x * scalar, this.y * scalar)
  }

  /**
   * Transforms this vector to one of (1, 0), (-1, 0), (0, 1), (0, -1).
   * The one chosen will be the one with the smallest angular distance.
   */
  toCardinalDirection () {
    if (Math.abs(this.x) > Math.abs(this.y)) {
      this.x = Math.sign(this.x)
      this.y = 0
    } else {
      this.x = 0
      this.y = Math.sign(this.y)
    }
  }

  toSerializable () : any {
    return { x: this.x, y: this.y }
  }

  static fromSerializable (data: any) : Point {
    return new Point(data.x, data.y)
  }
}

export class Rectangle {
  min : Point
  max : Point

  constructor (minx: number, miny: number, maxx: number, maxy: number) {
    this.min = new Point(minx, miny)
    this.max = new Point(maxx, maxy)
  }

  contains (p: Point) {
    return this.min.x < p.x && this.min.y < p.y && this.max.x > p.x && this.max.y > p.y
  }
}

export class Color {
  r: number = 0
  g: number = 0
  b: number = 0

  constructor (r: number, g: number, b: number) {
    this.r = r
    this.g = g
    this.b = b
  }

  toHex () {
    return '#' + this.r.toString(16).padStart(2, '0') + this.g.toString(16).padStart(2, '0') + this.b.toString(16).padStart(2, '0')
  }
}

export class Token {
  x: number = 0;
  y: number = 0;
  radius: number = 0.25;
  id: number = -1;
  isFoe: boolean = false
  color: Color = new Color(255, 0, 255)

  rotation: number = 0

  name: string = ''

  // Rendering variables
  displayX: number = 0
  displayY: number = 0
  displaySpeed: number = 8
}

export class Line {
  id: number = -1
  start: Point = new Point(0, 0)
  stop: Point = new Point(0, 0)
}

export const TOKEN_COLORS = [
  new Color(240, 50, 50), //  red
  new Color(176, 30, 90), //  burgund
  new Color(201, 20, 201), // pink
  new Color(120, 61, 196), // purple
  new Color(24, 100, 171), // blue
  new Color(24, 172, 171), // turquoise
  new Color(8, 127, 91), //   blue-green
  new Color(92, 148, 13), //  red-green
  new Color(217, 72, 15), //  orange
  new Color(129, 96, 65), //   brown
  new Color(201, 201, 30) //   yellow
]

// Event specific data types
export class TokenMoveOrder {
  x: number = 0
  y: number = 0
  rotation: number = 0
  token: Token = new Token()
}
