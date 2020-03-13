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

  render (ctx: CanvasRenderingContext2D, pass: number = 2) {
    // ctx.strokeStyle = '#FFFFFF'
    // ctx.fillStyle = '#444444'
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

  render (ctx: CanvasRenderingContext2D, pass: number = 2, isGm: boolean = true) {
    // if (!this.isVisible && !isGm) {
    //   return
    // }
    // if (pass === 2) {
    //   let initAlpha = ctx.globalAlpha
    //   if (!this.isVisible) {
    //     ctx.globalAlpha = 0.5
    //   }

    //   // Determine the start and stop along the wall
    //   let left = new Sim.Point(-this.facing.y, this.facing.x)
    //   let start = new Sim.Point(this.position.x + left.x * this.width / 2, this.position.y + left.y * this.width / 2)
    //   let stop = new Sim.Point(this.position.x - left.x * this.width / 2, this.position.y - left.y * this.width / 2)

    //   // replace the wall
    //   ctx.fillStyle = '#444444'
    //   let wt2 = (WALL_THICKNESS / 2 + WALL_OUTLINE) * this.facing.x
    //   let ht2 = (WALL_THICKNESS / 2 + WALL_OUTLINE) * this.facing.y

    //   ctx.fillRect(start.x - wt2, start.y - ht2, stop.x - start.x + 2 * wt2, stop.y - start.y + 2 * ht2)

    //   // Draw the door
    //   if (!this.isOpen) {
    //     ctx.strokeStyle = '#666666'
    //   } else {
    //     ctx.strokeStyle = '#FFFFFF'
    //   }
    //   ctx.beginPath()
    //   ctx.moveTo(stop.x, stop.y)
    //   ctx.lineTo(stop.x + this.facing.x * this.width, stop.y + this.facing.y * this.width)
    //   ctx.arcTo(start.x + this.facing.x * this.width, start.y + this.facing.y * this.width, start.x, start.y, this.width)
    //   ctx.stroke()
    //   if (!this.isOpen) {
    //     ctx.strokeStyle = '#FFFFFF'
    //     let l = ctx.lineWidth
    //     ctx.lineWidth *= 2
    //     ctx.beginPath()
    //     ctx.moveTo(start.x, start.y)
    //     ctx.lineTo(stop.x, stop.y)
    //     ctx.stroke()
    //     ctx.lineWidth = l
    //   }
    //   ctx.globalAlpha = initAlpha
    // }
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

  // constructor (id: number, start: Sim.Point, stop: Sim.Point) {
  //   this.id = id
  //   this.start = start
  //   this.end = stop
  // }

  render (ctx: CanvasRenderingContext2D, pass: number = 0, isGm: boolean) {
    // if (pass === 0) {
    //   // draw the outline
    //   ctx.fillStyle = '#111111'
    //   let wt2 = (WALL_THICKNESS / 2 + WALL_OUTLINE) * (this.start.x < this.stop.x ? 1 : -1)
    //   let ht2 = (WALL_THICKNESS / 2 + WALL_OUTLINE) * (this.start.y < this.stop.y ? 1 : -1)

    //   ctx.fillRect(this.start.x - wt2, this.start.y - ht2, this.stop.x - this.start.x + 2 * wt2, this.stop.y - this.start.y + 2 * ht2)
    // } else if (pass === 1) {
    //   // draw the center
    //   ctx.fillStyle = '#222222'
    //   let wt2 = WALL_THICKNESS / 2 * (this.start.x < this.stop.x ? 1 : -1)
    //   let ht2 = WALL_THICKNESS / 2 * (this.start.y < this.stop.y ? 1 : -1)

    //   ctx.fillRect(this.start.x - wt2, this.start.y - ht2, this.stop.x - this.start.x + 2 * wt2, this.stop.y - this.start.y + 2 * ht2)
    // }
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

  render (ctx: CanvasRenderingContext2D, pass: number = 0, isGm: boolean = true) {
    // if (!this.isVisible && !isGm) {
    //   return
    // }
    // let initAlpha = ctx.globalAlpha
    // if (!this.isVisible) {
    //   ctx.globalAlpha = 0.5
    // }
    // if (pass === 0) {
    //   // Draw the floor
    //   ctx.fillStyle = '#444444'
    //   ctx.fillRect(this.min.x, this.min.y, this.width(), this.height())

    //   ctx.strokeStyle = '#FFFFFF'
    //   ctx.beginPath()
    //   ctx.moveTo(this.min.x, this.min.y)
    //   ctx.lineTo(this.max.x, this.min.y)
    //   ctx.lineTo(this.max.x, this.max.y)
    //   ctx.lineTo(this.min.x, this.max.y)
    //   ctx.lineTo(this.min.x, this.min.y)
    //   ctx.stroke()
    // }
    // // Draw the walls
    // this.walls.forEach(wall => {
    //   wall.render(ctx, pass)
    // })

    // // Draw the furniture
    // this.furniture.forEach(f => {
    //   f.render(ctx, pass)
    // })

    // ctx.globalAlpha = initAlpha
  }

  width () : number {
    return this.size.x
  }

  height () : number {
    return this.size.y
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

  render (ctx: CanvasRenderingContext2D, isGm: boolean = false) {
    this.walls.forEach(wall => {
      wall.render(ctx, 0, isGm)
    })
    // for (let pass = 0; pass < 3; ++pass) {
    //   this.rooms.forEach(room => {
    //     room.render(ctx, pass, isGm)
    //   })
    //   this.doors.forEach(door => {
    //     door.render(ctx, pass, isGm)
    //   })
    // }
  }

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
