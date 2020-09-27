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

import * as B from '../simulation/building'
import * as Sim from '../simulation/simulation'
import eventBus from '../eventbus'
import PacketDispatcher from './packetdispatcher'

export default class BuildingServer {
  building: B.Building | null = null
  dispatcher: PacketDispatcher
  uid: string

  constructor (dispatcher: PacketDispatcher, uid: string) {
    this.dispatcher = dispatcher
    this.uid = uid

    eventBus.$on('/client/building/set', (data: B.Building) => { this.onClientSetBuilding(data) })

    eventBus.$on('/client/building/room/create', (data: B.Room) => { this.onClientCreateRoom(data) })
    eventBus.$on('/client/building/room/delete', (data: Sim.Point) => { this.onClientDeleteRoom(data) })
    eventBus.$on('/client/building/wall/create', (data: B.Wall) => { this.onClientCreateWall(data) })
    eventBus.$on('/client/building/wall/delete', (data: Sim.Point) => { this.onClientDeleteWall(data) })
    eventBus.$on('/client/building/door/create', (data: B.Door) => { this.onClientCreateDoor(data) })
    eventBus.$on('/client/building/door/delete', (data: Sim.Point) => { this.onClientDeleteDoor(data) })
    eventBus.$on('/client/building/door/toggle', (data: Sim.Point) => { this.onClientToggleDoor(data) })
    eventBus.$on('/client/building/furniture/create', (data: B.Furniture) => { this.onClientCreateFurniture(data) })
    eventBus.$on('/client/building/furniture/delete', (data: Sim.Point) => { this.onClientDeleteFurniture(data) })

    eventBus.$on('/client/building/save', () => { this.onClientSaveBuilding() })
    eventBus.$on('/client/building/load', (file: File) => { this.onClientLoadBuilding(file) })
    eventBus.$on('/client/building/clear', () => { this.onClientClearBuilding() })

    eventBus.$on('/client/building/reveal', (data: Sim.Rectangle) => { this.onClientReveal(data) })
  }

  onmessage (type: string, data: any): boolean {
    if (type === 'SetBuilding') {
      this.onServerSetBuilding(data)
    } else if (type === 'CreateRoom') {
      this.onServerCreateRoom(data)
    } else if (type === 'DeleteRoom') {
      this.onServerDeleteRoom(data)
    } else if (type === 'ModifyRoom') {
      this.onServerModifyRoom(data)
    } else if (type === 'CreateWall') {
      this.onServerCreateWall(data)
    } else if (type === 'DeleteWall') {
      this.onServerDeleteWall(data)
    } else if (type === 'ModifyWall') {
      this.onServerModifyWall(data)
    } else if (type === 'CreateDoor') {
      this.onServerCreateDoor(data)
    } else if (type === 'DeleteDoor') {
      this.onServerDeleteDoor(data)
    } else if (type === 'ModifyDoor') {
      this.onServerModifyDoor(data)
    } else if (type === 'CreateFurniture') {
      this.onServerCreateFurniture(data)
    } else if (type === 'DeleteFurniture') {
      this.onServerDeleteFurniture(data)
    } else if (type === 'ModifyFurniture') {
      this.onServerModifyFurniture(data)
    } else if (type === 'ClearBuilding') {
      this.onServerClearBuilding()
    } else if (type === 'LoadBuilding') {
      this.onServerLoadBuilding(data)
    }
    return false
  }

  // Handle the init packet
  onServerInit (data: any) {
    if (data.building === null) {
      this.building = null
    } else {
      this.building = B.Building.fromSerializable(data.building)
    }
  }

  onServerSetBuilding (data: any) {
    let b = null
    if (data !== null) {
      b = B.Building.fromSerializable(data)
    }
    this.building = b
    eventBus.$emit('/server/building/set', b)
  }

  onClientSetBuilding (data: B.Building) {
    let packet = {
      type: 'SetBuilding',
      uid: this.uid,
      data: data.toSerializable()
    }
    this.dispatcher.send(JSON.stringify(packet))
  }

  onClientCreateRoom (room: B.Room) {
    let packet = {
      type: 'CreateRoom',
      uid: this.uid,
      data: room.toSerializable()
    }
    this.dispatcher.send(JSON.stringify(packet))
  }

  onServerCreateRoom (data: any) {
    if (this.building === null) {
      this.building = new B.Building()
    }
    let room = B.Room.fromSerializable(data)
    this.building.rooms.push(room)
    eventBus.$emit('/server/building/room/create', room)
  }

  onClientDeleteRoom (p: Sim.Point) {
    if (this.building !== null) {
      this.building.rooms.forEach((r: B.Room) => {
        if (r.contains(p.x, p.y)) {
          let packet = {
            type: 'DeleteRoom',
            uid: this.uid,
            data: {
              'id': r.id
            }
          }
          this.dispatcher.send(JSON.stringify(packet))
        }
      })
    }
  }

  onServerDeleteRoom (data: any) {
    let id: number = data.id
    if (this.building !== null) {
      let i = this.building.rooms.findIndex((r: B.Room) => r.id === id)
      if (i !== -1) {
        eventBus.$emit('/server/building/room/delete', this.building.rooms[i])
        this.building.rooms.splice(i)
      }
    }
  }

  onServerModifyRoom (data: any) {
    if (this.building === null) {
      return
    }
    let newRoom = B.Room.fromSerializable(data)
    let oldRoom = this.building.rooms.find((other: B.Room) => other.id === newRoom.id)
    if (oldRoom !== undefined) {
      oldRoom.modify(newRoom)
      eventBus.$emit('/server/building/room/modified', oldRoom)
    }
  }

  onClientCreateWall (Wall: B.Wall) {
    let packet = {
      type: 'CreateWall',
      uid: this.uid,
      data: Wall.toSerializable()
    }
    this.dispatcher.send(JSON.stringify(packet))
  }

  onServerCreateWall (data: any) {
    if (this.building === null) {
      this.building = new B.Building()
    }
    let Wall = B.Wall.fromSerializable(data)
    this.building.walls.push(Wall)
    eventBus.$emit('/server/building/wall/create', Wall)
  }

  onClientDeleteWall (p: Sim.Point) {
    if (this.building !== null) {
      this.building.walls.forEach((r: B.Wall) => {
        if (r.distTo(p.x, p.y) < 0.5) {
          let packet = {
            type: 'DeleteWall',
            uid: this.uid,
            data: {
              'id': r.id
            }
          }
          this.dispatcher.send(JSON.stringify(packet))
        }
      })
    }
  }

  onServerDeleteWall (data: any) {
    let id: number = data.id
    if (this.building !== null) {
      let i = this.building.walls.findIndex((r: B.Wall) => r.id === id)
      if (i !== -1) {
        eventBus.$emit('/server/building/wall/delete', this.building.walls[i])
        this.building.walls.splice(i)
      }
    }
  }

  onServerModifyWall (data: any) {
    if (this.building === null) {
      return
    }
    let newWall = B.Wall.fromSerializable(data)
    let oldWall = this.building.walls.find((other: B.Wall) => other.id === newWall.id)
    if (oldWall !== undefined) {
      oldWall.modify(newWall)
      eventBus.$emit('/server/building/wall/modified', oldWall)
    }
  }

  onClientCreateDoor (Door: B.Door) {
    let packet = {
      type: 'CreateDoor',
      uid: this.uid,
      data: Door.toSerializable()
    }
    this.dispatcher.send(JSON.stringify(packet))
  }

  onServerCreateDoor (data: any) {
    if (this.building === null) {
      this.building = new B.Building()
    }
    let Door = B.Door.fromSerializable(data)
    this.building.doors.push(Door)
    eventBus.$emit('/server/building/door/create', Door)
  }

  onClientDeleteDoor (p: Sim.Point) {
    if (this.building !== null) {
      this.building.doors.forEach((r: B.Door) => {
        if (r.position.distTo(p) < 0.5) {
          let packet = {
            type: 'DeleteDoor',
            uid: this.uid,
            data: {
              'id': r.id
            }
          }
          this.dispatcher.send(JSON.stringify(packet))
        }
      })
    }
  }

  onServerDeleteDoor (data: any) {
    let id: number = data.id
    if (this.building !== null) {
      let i = this.building.doors.findIndex((r: B.Door) => r.id === id)
      if (i !== -1) {
        eventBus.$emit('/server/building/door/delete', this.building.doors[i])
        this.building.doors.splice(i)
      }
    }
  }

  onClientToggleDoor (p: Sim.Point) {
    if (this.building !== null) {
      this.building.doors.forEach((r: B.Door) => {
        if (r.position.distTo(p) < 0.5) {
          let packet = {
            type: 'ModifyDoor',
            uid: this.uid,
            data: r.toSerializable()
          }
          packet.data.is_open = !packet.data.is_open
          this.dispatcher.send(JSON.stringify(packet))
        }
      })
    }
  }

  onServerModifyDoor (data: any) {
    if (this.building === null) {
      return
    }
    let door = B.Door.fromSerializable(data)
    let d = this.building.doors.find((od: B.Door) => od.id === door.id)
    if (d !== undefined) {
      d.modify(door)
      eventBus.$emit('/server/building/door/modified', d)
    }
  }

  onClientCreateFurniture (Furniture: B.Furniture) {
    let packet = {
      type: 'CreateFurniture',
      uid: this.uid,
      data: Furniture.toSerializable()
    }
    this.dispatcher.send(JSON.stringify(packet))
  }

  onServerCreateFurniture (data: any) {
    if (this.building === null) {
      this.building = new B.Building()
    }
    let Furniture = B.Furniture.fromSerializable(data)
    this.building.furniture.push(Furniture)
    eventBus.$emit('/server/building/furniture/create', Furniture)
  }

  onClientDeleteFurniture (p: Sim.Point) {
    if (this.building !== null) {
      this.building.furniture.forEach((r: B.Furniture) => {
        if (r.position.distTo(p) < 0.5) {
          let packet = {
            type: 'DeleteFurniture',
            uid: this.uid,
            data: {
              'id': r.id
            }
          }
          this.dispatcher.send(JSON.stringify(packet))
        }
      })
    }
  }

  onServerDeleteFurniture (data: any) {
    let id: number = data.id
    if (this.building !== null) {
      let i = this.building.furniture.findIndex((r: B.Furniture) => r.id === id)
      if (i !== -1) {
        eventBus.$emit('/server/building/furniture/delete', this.building.furniture[i])
        this.building.furniture.splice(i)
      }
    }
  }

  onServerModifyFurniture (data: any) {
    if (this.building === null) {
      return
    }
    let Furniture = B.Furniture.fromSerializable(data)
    let d = this.building.furniture.find((od: B.Furniture) => od.id === Furniture.id)
    if (d !== undefined) {
      d.modify(Furniture)
      eventBus.$emit('/server/building/furniture/modified', d)
    }
  }

  onClientReveal (area: Sim.Rectangle) {
    if (this.building === null) {
      return
    }
    this.building.rooms.forEach((r: B.Room) => {
      if (area.contains(r.position)) {
        let packet = {
          type: 'ModifyRoom',
          uid: this.uid,
          data: r.toSerializable()
        }
        packet.data.is_visible = !packet.data.is_visible
        this.dispatcher.send(JSON.stringify(packet))
      }
    })
    this.building.doors.forEach((r: B.Door) => {
      if (area.contains(r.position)) {
        let packet = {
          type: 'ModifyDoor',
          uid: this.uid,
          data: r.toSerializable()
        }
        packet.data.is_visible = !packet.data.is_visible
        this.dispatcher.send(JSON.stringify(packet))
      }
    })
    this.building.furniture.forEach((r: B.Furniture) => {
      if (area.contains(r.position)) {
        let packet = {
          type: 'ModifyFurniture',
          uid: this.uid,
          data: r.toSerializable()
        }
        packet.data.is_visible = !packet.data.is_visible
        this.dispatcher.send(JSON.stringify(packet))
      }
    })
    this.building.walls.forEach((r: B.Wall) => {
      if (area.contains(r.start) || area.contains(r.end)) {
        let packet = {
          type: 'ModifyWall',
          uid: this.uid,
          data: r.toSerializable()
        }
        packet.data.is_visible = !packet.data.is_visible
        this.dispatcher.send(JSON.stringify(packet))
      }
    })
  }

  onClientClearBuilding () {
    let packet = {
      type: 'ClearBuilding',
      uid: this.uid,
      data: null
    }
    this.dispatcher.send(JSON.stringify(packet))
  }

  onServerClearBuilding () {
    this.building = new B.Building()
    eventBus.$emit('/server/building/clear')
  }

  onClientSaveBuilding () {
    if (this.building !== null) {
      let data = new Blob([JSON.stringify(this.building.toSerializable())], { type: 'application/json' })
      let fileSaver = require('../lib/filesaver/FileSaver.min.js')
      fileSaver.saveAs(data, 'Building.json')
    }
  }

  onClientLoadBuilding (file: File) {
    let reader: FileReader = new FileReader()
    reader.onload = (raw: any) => {
      if (typeof reader.result === 'string') {
        let data = JSON.parse(reader.result as string)
        let b = B.Building.fromSerializable(data)
        let packet = {
          type: 'LoadBuilding',
          uid: this.uid,
          data: b.toSerializable()
        }
        this.dispatcher.send(JSON.stringify(packet))
      }
    }
    reader.readAsText(file)
  }

  onServerLoadBuilding (data: any) {
    eventBus.$emit('/server/building/clear')
    this.building = B.Building.fromSerializable(data)
    eventBus.$emit('/server/building/load', this.building)
  }
}
