import * as Sim from '../simulation/simulation'

const WALL_THICKNESS = 0.17
const WALL_OUTLINE = 0.05

export class Door {
  position: Sim.Point = new Sim.Point(0, 0)
  facing: Sim.Point = new Sim.Point(0, 0)
  width: number = 1
  id: number = -1
  isOpen: boolean = true

  render (ctx: CanvasRenderingContext2D, pass: number = 2) {
    if (pass === 2) {
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
    }
  }

  toSerializable () : any {
    return {
      position: this.position.toSerializable(),
      facing: this.facing.toSerializable(),
      width: this.width,
      id: this.id,
      isOpen: this.isOpen
    }
  }

  static fromSerializable (data: any) : Door {
    let d = new Door()
    d.position = Sim.Point.fromSerializable(data.position)
    d.facing = Sim.Point.fromSerializable(data.facing)
    d.width = data.width
    d.id = data.id
    d.isOpen = data.isOpen
    return d
  }
}

export class Wall {
  start: Sim.Point = new Sim.Point(0, 0)
  stop: Sim.Point = new Sim.Point(0, 0)

  constructor (start: Sim.Point, stop: Sim.Point) {
    this.start = start
    this.stop = stop
  }

  render (ctx: CanvasRenderingContext2D, pass: number = 0) {
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
  }

  toSerializable () : any {
    return {
      start: this.start.toSerializable(),
      stop: this.stop.toSerializable()
    }
  }

  static fromSerializable (data: any) : Wall {
    return new Wall(Sim.Point.fromSerializable(data.start), Sim.Point.fromSerializable(data.stop))
  }
}

export class Room {
  min: Sim.Point = new Sim.Point(0, 0)
  max: Sim.Point = new Sim.Point(0, 0)

  id: number = -1

  walls: Wall[] = []

  buildWalls () {
    this.walls.push(new Wall(new Sim.Point(this.min.x, this.min.y), new Sim.Point(this.max.x, this.min.y)))
    this.walls.push(new Wall(new Sim.Point(this.max.x, this.min.y), new Sim.Point(this.max.x, this.max.y)))
    this.walls.push(new Wall(new Sim.Point(this.max.x, this.max.y), new Sim.Point(this.min.x, this.max.y)))
    this.walls.push(new Wall(new Sim.Point(this.min.x, this.max.y), new Sim.Point(this.min.x, this.min.y)))
  }

  width () : number {
    return this.max.x - this.min.x
  }

  height () : number {
    return this.max.y - this.min.y
  }

  render (ctx: CanvasRenderingContext2D, pass: number = 0) {
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
    // Draw the walls
    this.walls.forEach(wall => {
      wall.render(ctx, pass)
    })
  }

  contains (x: number, y: number) : boolean {
    return x >= this.min.x && x <= this.max.x && y >= this.min.y && y <= this.max.y
  }

  toSerializable () : any {
    return {
      min: this.min.toSerializable(),
      max: this.max.toSerializable(),
      id: this.id,
      walls: this.walls.map(wall => wall.toSerializable())
    }
  }

  static fromSerializable (data: any) : Room {
    let r = new Room()
    r.min = Sim.Point.fromSerializable(data.min)
    r.max = Sim.Point.fromSerializable(data.max)
    r.id = data.id
    r.walls = data.walls.map(wdata => Wall.fromSerializable(wdata))
    return r
  }
}

export class Building {
  rooms: Room[] = []
  doors: Door[] = []
  nextId: number = 0

  render (ctx: CanvasRenderingContext2D) {
    for (let pass = 0; pass < 3; ++pass) {
      this.rooms.forEach(room => {
        room.render(ctx, pass)
      })
      this.doors.forEach(door => {
        door.render(ctx, pass)
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
    b.rooms = data.rooms.map(rdata => Room.fromSerializable(rdata))
    b.doors = data.doors.map(ddata => Door.fromSerializable(ddata))
    b.nextId = data.nextId
    return b
  }
}
