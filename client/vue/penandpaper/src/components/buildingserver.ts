import * as B from '../simulation/building'
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
    eventBus.$on('/client/building/clear', () => { this.onClientClearBuilding() })
    eventBus.$on('/client/building/toggle_door', (data: B.Door[]) => { this.onClientToggleDoor(data) })

    eventBus.$on('/client/building/room/create', (data: B.Room) => { this.onClientCreateRoom(data) })
  }

  onmessage (msg: MessageEvent): boolean {
    let packet = JSON.parse(msg.data)
    let type = packet['type']
    let data = packet['data']
    if (type === 'SetBuilding') {
      this.onServerSetBuilding(data)
    } else if (type === 'CreateRoom') {
      this.onServerCreateRoom(data)
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

  onClientClearBuilding () {
    let packet = {
      type: 'SetBuilding',
      uid: this.uid,
      data: null
    }
    this.dispatcher.send(JSON.stringify(packet))
  }

  onClientToggleDoor (doors: B.Door[]) {
    let packet = {
      type: 'ToggleDoor',
      uid: this.uid,
      data: {
        ids: doors.map(door => door.id)
      }
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
    // TODO: nothing currently listens to this event
    eventBus.$emit('/server/building/room/create', room)
  }
}
