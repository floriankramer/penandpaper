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

import * as Sim from '../simulation/simulation'

const WALL_THICKNESS = 0.17
const WALL_OUTLINE = 0.05

export class Furniture {
  id: number = -1
  position: Sim.Point = new Sim.Point(0, 0)
  size: Sim.Point = new Sim.Point(0, 0)
  rotation: number = 0
  isVisible: boolean = false

  modify (of: Furniture) {
    this.position.x = of.position.x
    this.position.y = of.position.y
    this.size.x = of.size.x
    this.size.y = of.size.y
    this.rotation = of.rotation
    this.isVisible = of.isVisible
  }

  contains (pos: Sim.Point) {
    let c = this.position
    // rotate the point by -rotation around c
    let r = new Sim.Point(pos.x - c.x, pos.y - c.y)
    let tmpx = r.x * Math.cos(-this.rotation) - r.y * Math.sin(-this.rotation)
    r.y = r.x * Math.sin(-this.rotation) + r.y * Math.cos(-this.rotation)
    r.x = tmpx
    r.x += c.x
    r.y += c.y
    return r.x >= c.x - this.size.x / 2 &&
           r.x <= c.x + this.size.x / 2 &&
           r.y >= c.y - this.size.y / 2 &&
           r.y <= c.y + this.size.y / 2
  }

  width () : number {
    return this.size.x
  }

  height () : number {
    return this.size.y
  }

  setCenter (pos: Sim.Point) {
    this.position.x = pos.x
    this.position.y = pos.y
  }

  toSerializable () : any {
    return {
      position: this.position.toSerializable(),
      size: this.size.toSerializable(),
      id: this.id,
      rotation: this.rotation,
      is_visible: this.isVisible
    }
  }

  static fromSerializable (data: any) : Furniture {
    let f = new Furniture()
    f.position = Sim.Point.fromSerializable(data.position)
    f.size = Sim.Point.fromSerializable(data.size)
    f.id = data.id
    f.rotation = data.rotation
    f.isVisible = data.is_visible
    return f
  }
}

export class Door {
  id: number = -1
  position: Sim.Point = new Sim.Point(0, 0)
  width: number = 1
  rotation: number = 0
  isVisible: boolean = false
  isOpen: boolean = true

  modify (od: Door) {
    this.position.x = od.position.x
    this.position.y = od.position.y
    this.width = od.width
    this.rotation = od.rotation
    this.isVisible = od.isVisible
    this.isOpen = od.isOpen
  }

  toSerializable () : any {
    return {
      position: this.position.toSerializable(),
      rotation: this.rotation,
      width: this.width,
      id: this.id,
      is_open: this.isOpen,
      is_visible: this.isVisible
    }
  }

  static fromSerializable (data: any) : Door {
    let d = new Door()
    d.position = Sim.Point.fromSerializable(data.position)
    d.rotation = data.rotation
    d.width = data.width
    d.id = data.id
    d.isOpen = data.is_open
    d.isVisible = data.is_visible
    return d
  }
}

export class Wall {
  id: number = -1
  start: Sim.Point = new Sim.Point(0, 0)
  end: Sim.Point = new Sim.Point(0, 0)
  isVisible: boolean = false

  modify (of: Wall) {
    this.start.x = of.start.x
    this.start.y = of.start.y
    this.end.x = of.end.x
    this.end.y = of.end.y
    this.isVisible = of.isVisible
  }

  length (): number {
    return Math.hypot(this.start.x - this.end.x, this.start.y - this.end.y)
  }

  distTo (x: number, y: number): number {
    let p = new Sim.Point(x, y)
    let delta = this.end.minus(this.start)
    let length = delta.length()
    delta.normalize()
    let pdelta = p.minus(this.start)
    // Project the vector start->p onto the line described by this wall.
    // Check if the projected point is outsuide of the wall and return
    // the distance to an endpoint or to the projected point
    let d = delta.dot(pdelta)
    if (d < 0) {
      return pdelta.length()
    } else if (d > length) {
      return p.minus(this.end).length()
    } else {
      return p.minus(this.start.add(delta.scale(d))).length()
    }
  }

  toSerializable () : any {
    return {
      id: this.id,
      start: this.start.toSerializable(),
      end: this.end.toSerializable(),
      is_visible: this.isVisible
    }
  }

  static fromSerializable (data: any) : Wall {
    let w = new Wall()
    w.id = data.id
    w.start = Sim.Point.fromSerializable(data.start)
    w.end = Sim.Point.fromSerializable(data.end)
    w.isVisible = data.is_visible
    return w
  }
}

export class Room {
  id: number = -1
  position: Sim.Point = new Sim.Point(0, 0)
  size: Sim.Point = new Sim.Point(0, 0)
  isVisible: boolean = false

  modify (of: Room) {
    this.position.x = of.position.x
    this.position.y = of.position.y
    this.size = of.size
    this.isVisible = of.isVisible
  }

  width () : number {
    return this.size.x * 2
  }

  height () : number {
    return this.size.y * 2
  }

  contains (x: number, y: number, margin: number = 0) : boolean {
    return x >= this.position.x - this.size.x / 2 - margin &&
           x <= this.position.x + this.size.x / 2 + margin &&
           y >= this.position.y - this.size.y / 2 - margin &&
           y <= this.position.y + this.size.y / 2 + margin
  }

  toSerializable () : any {
    return {
      id: this.id,
      position: this.position.toSerializable(),
      size: this.size.toSerializable(),
      is_visible: this.isVisible
    }
  }

  static fromSerializable (data: any) : Room {
    let r = new Room()
    r.id = data.id
    r.position = Sim.Point.fromSerializable(data.position)
    r.size = Sim.Point.fromSerializable(data.size)
    r.isVisible = data.is_visible
    return r
  }
}

export class Building {
  id: number = -1
  rooms: Room[] = []
  doors: Door[] = []
  walls: Wall[] = []
  furniture: Furniture[] = []

  toSerializable () : any {
    return {
      id: this.id,
      rooms: this.rooms.map(room => room.toSerializable()),
      walls: this.walls.map(wall => wall.toSerializable()),
      doors: this.doors.map(door => door.toSerializable()),
      furniture: this.furniture.map(furniture => furniture.toSerializable())
    }
  }

  static fromSerializable (data: any) : Building {
    console.log(data)
    let b = new Building()
    b.id = data.id
    b.rooms = data.rooms.map((rdata: any) => Room.fromSerializable(rdata))
    b.walls = data.walls.map((rdata: any) => Wall.fromSerializable(rdata))
    b.doors = data.doors.map((ddata: any) => Door.fromSerializable(ddata))
    b.furniture = data.furniture.map((rdata: any) => Furniture.fromSerializable(rdata))
    return b
  }
}
