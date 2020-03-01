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
  min: Sim.Point = new Sim.Point(0, 0)
  max: Sim.Point = new Sim.Point(0, 0)
  id: number = -1
  rotation: number = 0

  render (ctx: CanvasRenderingContext2D, pass: number = 2) {
    ctx.strokeStyle = '#FFFFFF'
    ctx.fillStyle = '#444444'
    let t = ctx.getTransform()
    let c = this.center()
    ctx.translate(c.x, c.y)
    ctx.rotate(this.rotation)
    ctx.translate(-c.x, -c.y)
    ctx.fillRect(this.min.x, this.min.y, this.max.x - this.min.x, this.max.y - this.min.y)
    ctx.strokeRect(this.min.x, this.min.y, this.max.x - this.min.x, this.max.y - this.min.y)
    ctx.setTransform(t)
  }

  contains (pos: Sim.Point) {
    let c = this.center()
    // rotate the point by -rotation around c
    let r = new Sim.Point(pos.x - c.x, pos.y - c.y)
    let tmpx = r.x * Math.cos(-this.rotation) - r.y * Math.sin(-this.rotation)
    r.y = r.x * Math.sin(-this.rotation) + r.y * Math.cos(-this.rotation)
    r.x = tmpx
    r.x += c.x
    r.y += c.y
    return r.x >= this.min.x && r.x <= this.max.x && r.y >= this.min.y && r.y <= this.max.y
  }

  width () : number {
    return this.max.x - this.min.x
  }

  height () : number {
    return this.max.y - this.min.y
  }

  center () : Sim.Point {
    return new Sim.Point((this.min.x + this.max.x) / 2.0, (this.min.y + this.max.y) / 2.0)
  }

  setCenter (pos: Sim.Point) {
    let w = this.width() / 2
    let h = this.height() / 2
    this.min.x = pos.x - w
    this.max.x = pos.x + w
    this.min.y = pos.y - h
    this.max.y = pos.y + h
  }

  toSerializable () : any {
    return {
      min: this.min.toSerializable(),
      max: this.max.toSerializable(),
      id: this.id,
      rotation: this.rotation
    }
  }

  static fromSerializable (data: any) : Furniture {
    let f = new Furniture()
    f.min = Sim.Point.fromSerializable(data.min)
    f.max = Sim.Point.fromSerializable(data.max)
    f.id = data.id
    f.rotation = data.rotation
    return f
  }
}

export class Door {
  position: Sim.Point = new Sim.Point(0, 0)
  facing: Sim.Point = new Sim.Point(0, 0)
  width: number = 1
  id: number = -1
  isOpen: boolean = true
  isVisible: boolean = false

  render (ctx: CanvasRenderingContext2D, pass: number = 2, isGm: boolean = true) {
    if (!this.isVisible && !isGm) {
      return
    }
    if (pass === 2) {
      let initAlpha = ctx.globalAlpha
      if (!this.isVisible) {
        ctx.globalAlpha = 0.5
      }

      // Determine the start and stop along the wall
      let left = new Sim.Point(-this.facing.y, this.facing.x)
      let start = new Sim.Point(this.position.x + left.x * this.width / 2, this.position.y + left.y * this.width / 2)
      let stop = new Sim.Point(this.position.x - left.x * this.width / 2, this.position.y - left.y * this.width / 2)

      // replace the wall
      ctx.fillStyle = '#444444'
      let wt2 = (WALL_THICKNESS / 2 + WALL_OUTLINE) * this.facing.x
      let ht2 = (WALL_THICKNESS / 2 + WALL_OUTLINE) * this.facing.y

      ctx.fillRect(start.x - wt2, start.y - ht2, stop.x - start.x + 2 * wt2, stop.y - start.y + 2 * ht2)

      // Draw the door
      if (!this.isOpen) {
        ctx.strokeStyle = '#666666'
      } else {
        ctx.strokeStyle = '#FFFFFF'
      }
      ctx.beginPath()
      ctx.moveTo(stop.x, stop.y)
      ctx.lineTo(stop.x + this.facing.x * this.width, stop.y + this.facing.y * this.width)
      ctx.arcTo(start.x + this.facing.x * this.width, start.y + this.facing.y * this.width, start.x, start.y, this.width)
      ctx.stroke()
      if (!this.isOpen) {
        ctx.strokeStyle = '#FFFFFF'
        let l = ctx.lineWidth
        ctx.lineWidth *= 2
        ctx.beginPath()
        ctx.moveTo(start.x, start.y)
        ctx.lineTo(stop.x, stop.y)
        ctx.stroke()
        ctx.lineWidth = l
      }
      ctx.globalAlpha = initAlpha
    }
  }

  toSerializable () : any {
    return {
      position: this.position.toSerializable(),
      facing: this.facing.toSerializable(),
      width: this.width,
      id: this.id,
      isOpen: this.isOpen,
      isVisible: this.isVisible
    }
  }

  static fromSerializable (data: any) : Door {
    let d = new Door()
    d.position = Sim.Point.fromSerializable(data.position)
    d.facing = Sim.Point.fromSerializable(data.facing)
    d.width = data.width
    d.id = data.id
    d.isOpen = data.isOpen
    d.isVisible = data.isVisible
    return d
  }
}

export class Wall {
  start: Sim.Point = new Sim.Point(0, 0)
  stop: Sim.Point = new Sim.Point(0, 0)
  visible: boolean = false

  constructor (start: Sim.Point, stop: Sim.Point) {
    this.start = start
    this.stop = stop
  }

  render (ctx: CanvasRenderingContext2D, pass: number = 0, isGm: boolean = true) {
    if (this.visible || isGm) {
      let initAlpha = ctx.globalAlpha
      if (!this.visible) {
        ctx.globalAlpha = 0.5
      }
      if (pass === 0) {
        // draw the outline
        ctx.fillStyle = '#111111'
        let wt2 = (WALL_THICKNESS / 2 + WALL_OUTLINE) * (this.start.x < this.stop.x ? 1 : -1)
        let ht2 = (WALL_THICKNESS / 2 + WALL_OUTLINE) * (this.start.y < this.stop.y ? 1 : -1)

        ctx.fillRect(this.start.x - wt2, this.start.y - ht2, this.stop.x - this.start.x + 2 * wt2, this.stop.y - this.start.y + 2 * ht2)
      } else if (pass === 1) {
        // draw the center
        ctx.fillStyle = '#222222'
        let wt2 = WALL_THICKNESS / 2 * (this.start.x < this.stop.x ? 1 : -1)
        let ht2 = WALL_THICKNESS / 2 * (this.start.y < this.stop.y ? 1 : -1)

        ctx.fillRect(this.start.x - wt2, this.start.y - ht2, this.stop.x - this.start.x + 2 * wt2, this.stop.y - this.start.y + 2 * ht2)
      }
      ctx.globalAlpha = initAlpha
    }
  }

  toSerializable () : any {
    return {
      start: this.start.toSerializable(),
      stop: this.stop.toSerializable(),
      visible: this.visible
    }
  }

  static fromSerializable (data: any) : Wall {
    let w =  new Wall(Sim.Point.fromSerializable(data.start), Sim.Point.fromSerializable(data.stop))
    if ('visible' in data) {
      w.visible = data.visible
    }
    return w
  }
}

export class Room {
  min: Sim.Point = new Sim.Point(0, 0)
  max: Sim.Point = new Sim.Point(0, 0)
  isVisible: boolean = false

  id: number = -1

  walls: Wall[] = []
  furniture: Furniture[] = []

  buildWalls () {
    this.walls.push(new Wall(new Sim.Point(this.min.x, this.min.y), new Sim.Point(this.max.x, this.min.y)))
    this.walls.push(new Wall(new Sim.Point(this.max.x, this.min.y), new Sim.Point(this.max.x, this.max.y)))
    this.walls.push(new Wall(new Sim.Point(this.max.x, this.max.y), new Sim.Point(this.min.x, this.max.y)))
    this.walls.push(new Wall(new Sim.Point(this.min.x, this.max.y), new Sim.Point(this.min.x, this.min.y)))
  }

  addFurniture (f: Furniture) {
    this.furniture.push(f)
  }

  removeFurnitureAt (pos: Sim.Point) {
    for (let i = 0; i < this.furniture.length; ++i) {
      if (this.furniture[i].contains(pos)) {
        this.furniture.splice(i, 1)
        i -= 1
      }
    }
  }

  getFurnitureAt (pos: Sim.Point) : Furniture | undefined {
    for (let i = 0; i < this.furniture.length; ++i) {
      if (this.furniture[i].contains(pos)) {
        return this.furniture[i]
      }
    }
    return undefined
  }

  width () : number {
    return this.max.x - this.min.x
  }

  height () : number {
    return this.max.y - this.min.y
  }

  center () : Sim.Point {
    return new Sim.Point((this.min.x + this.max.x) / 2.0, (this.min.y + this.max.y) / 2.0)
  }

  render (ctx: CanvasRenderingContext2D, pass: number = 0, isGm: boolean = true) {
    if (this.isVisible || isGm) {
      let initAlpha = ctx.globalAlpha
      if (!this.isVisible) {
        ctx.globalAlpha = 0.5
      }
      if (pass === 0) {
        // Draw the floor
        ctx.fillStyle = '#444444'
        ctx.fillRect(this.min.x, this.min.y, this.width(), this.height())

        ctx.strokeStyle = '#FFFFFF'
        ctx.beginPath()
        ctx.moveTo(this.min.x, this.min.y)
        ctx.lineTo(this.max.x, this.min.y)
        ctx.lineTo(this.max.x, this.max.y)
        ctx.lineTo(this.min.x, this.max.y)
        ctx.lineTo(this.min.x, this.min.y)
        ctx.stroke()
      }

      // Draw the furniture
      this.furniture.forEach(f => {
        f.render(ctx, pass)
      })
      ctx.globalAlpha = initAlpha
    }

    // Draw the walls. Walls could be independently visible
    this.walls.forEach(wall => {
      wall.render(ctx, pass, isGm)
    })
  }

  contains (x: number, y: number, margin: number = 0) : boolean {
    return x >= this.min.x - margin && x <= this.max.x + margin && y >= this.min.y - margin && y <= this.max.y + margin
  }

  toSerializable () : any {
    return {
      min: this.min.toSerializable(),
      max: this.max.toSerializable(),
      id: this.id,
      walls: this.walls.map(wall => wall.toSerializable()),
      furniture: this.furniture.map(f => f.toSerializable()),
      isVisible: this.isVisible
    }
  }

  static fromSerializable (data: any) : Room {
    let r = new Room()
    r.min = Sim.Point.fromSerializable(data.min)
    r.max = Sim.Point.fromSerializable(data.max)
    r.id = data.id
    r.walls = data.walls.map((wdata: any) => Wall.fromSerializable(wdata))
    r.furniture = data.furniture.map((fdata: any) => Furniture.fromSerializable(fdata))
    r.isVisible = data.isVisible
    return r
  }
}

export class Building {
  rooms: Room[] = []
  doors: Door[] = []
  nextId: number = 0

  render (ctx: CanvasRenderingContext2D, isGm: boolean = false) {
    for (let pass = 0; pass < 3; ++pass) {
      this.rooms.forEach(room => {
        room.render(ctx, pass, isGm)
      })
      this.doors.forEach(door => {
        door.render(ctx, pass, isGm)
      })
    }
  }

  addRoom (room: Room) {
    room.id = this.nextId
    this.nextId += 1

    room.buildWalls()

    this.rooms.push(room)
  }

  addDoor (door: Door) {
    door.id = this.nextId
    door.isOpen = false
    this.nextId += 1
    this.doors.push(door)
  }

  addFurniture (f: Furniture) {
    let r = this.getRoomAt(f.min)
    if (r !== undefined && r.contains(f.max.x, f.max.y)) {
      f.id = this.nextId
      this.nextId += 1
      r.addFurniture(f)
    }
  }

  removeFurnitureAt (pos: Sim.Point) {
    let r = this.getRoomAt(pos)
    if (r !== undefined) {
      r.removeFurnitureAt(pos)
    }
  }

  getFurnitureAt (pos: Sim.Point) : Furniture | undefined {
    let r = this.getRoomAt(pos)
    if (r !== undefined) {
      return r.getFurnitureAt(pos)
    }
    return undefined
  }

  removeRoomAt (x: number, y: number) {
    for (let i = 0; i < this.rooms.length; ++i) {
      if (this.rooms[i].contains(x, y)) {
        this.rooms.splice(i, 1)
        i -= 1
      }
    }
  }

  removeDoorAt (x: number, y: number) {
    let p = new Sim.Point(x, y)
    for (let i = 0; i < this.doors.length; ++i) {
      let d = Math.max(this.doors[i].width, 0.2)
      if (this.doors[i].position.distTo(p) < d) {
        this.doors.splice(i, 1)
        i -= 1
      }
    }
  }

  isDoorAt (x: number, y: number) : boolean {
    let p = new Sim.Point(x, y)
    for (let i = 0; i < this.doors.length; ++i) {
      let d = Math.max(this.doors[i].width, 0.2)
      if (this.doors[i].position.distTo(p) < d) {
        return true
      }
    }
    return false
  }

  getDoorsAt (x: number, y: number) : Door[] {
    let doors = []
    let p = new Sim.Point(x, y)
    for (let i = 0; i < this.doors.length; ++i) {
      let d = Math.max(this.doors[i].width, 0.2)
      if (this.doors[i].position.distTo(p) < d) {
        doors.push(this.doors[i])
      }
    }
    return doors
  }

  toggleDoors (ids: number[]) {
    console.log('toggeling doors: ', ids, this.doors)
    for (let i = 0; i < this.doors.length; ++i) {
      if (ids.find(j => j === this.doors[i].id) !== undefined) {
        this.doors[i].isOpen = !this.doors[i].isOpen
      }
    }
  }

  toggleDoorAt (x: number, y: number) : boolean {
    let p = new Sim.Point(x, y)
    let openedADoor = false
    for (let i = 0; i < this.doors.length; ++i) {
      let d = Math.max(this.doors[i].width, 0.2)
      if (this.doors[i].position.distTo(p) < d) {
        this.doors[i].isOpen = !this.doors[i].isOpen
        openedADoor = true
      }
    }
    return openedADoor
  }

  revealRoomsAt (pos: Sim.Point) {
    for (let i = 0; i < this.rooms.length; ++i) {
      if (this.rooms[i].contains(pos.x, pos.y)) {
        this.rooms[i].isVisible = true
        this.rooms[i].walls.forEach(w => {
          w.visible = true
        })
        for (let j = 0; j < this.doors.length; ++j) {
          let door = this.doors[j]
          if (this.rooms[i].contains(door.position.x, door.position.y, 0.2)) {
            this.doors[j].isVisible = true
          }
        }
      }
    }
  }

  revealWallAt (pos: Sim.Point) {
    let roomIdx = -1
    let wallIdx = -1
    let minDist = 5
    for (let i = 0; i < this.rooms.length; ++i) {
      for (let j = 0; j < this.rooms[i].walls.length; ++j) {
        let w = this.rooms[i].walls[j]
        let center = new Sim.Point((w.start.x + w.stop.x) / 2, (w.start.y + w.stop.y) / 2)
        let dist = Math.hypot(center.x - pos.x, center.y - pos.y)
        if (dist < minDist) {
          minDist = dist
          roomIdx = i
          wallIdx = j
        }
      }
    }
    if (roomIdx >= 0) {
      this.rooms[roomIdx].walls[wallIdx].visible = true
    }
  }

  getRoomAt (pos: Sim.Point) : Room | undefined {
    for (let i = 0; i < this.rooms.length; ++i) {
      if (this.rooms[i].contains(pos.x, pos.y)) {
        return this.rooms[i]
      }
    }
    return undefined
  }

  toSerializable () : any {
    return {
      rooms: this.rooms.map(room => room.toSerializable()),
      doors: this.doors.map(door => door.toSerializable()),
      nextId: this.nextId
    }
  }

  static fromSerializable (data: any) : Building {
    console.log(data)
    let b = new Building()
    b.rooms = data.rooms.map((rdata: any) => Room.fromSerializable(rdata))
    b.doors = data.doors.map((ddata: any) => Door.fromSerializable(ddata))
    b.nextId = data.nextId
    return b
  }
}
