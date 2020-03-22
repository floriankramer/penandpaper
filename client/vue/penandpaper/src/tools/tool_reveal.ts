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

import Tool from './tool'
import Map from '../components/Map.vue'
import * as Sim from '../simulation/simulation'
import eventBus from '../eventbus'
import * as B from '../simulation/building'
import Renderer from '../rendering/renderer'

import RoomActor from '../rendering/roomactor'

export default class ToolRoom extends Tool {
  isDrawing: boolean = false
  start: Sim.Point = new Sim.Point(0, 0)
  stop: Sim.Point = new Sim.Point(0, 0)

  currentRoom: B.Room = new B.Room()

  roomActor: RoomActor = new RoomActor()
  isActorVisible: boolean = false

  constructor (map: Map) {
    super(map)
    this.currentRoom.isVisible = true
    this.roomActor.addRoom(this.currentRoom)
  }

  onMouseDown (event: MouseEvent) : boolean {
    if (event.ctrlKey) {
      let worldPos = this.map.screenToWorldPos(new Sim.Point(event.offsetX, event.offsetY))
      this.start.x = worldPos.x
      this.start.y = worldPos.y
      this.stop.x = this.start.x
      this.stop.y = this.start.y
      this.isDrawing = true

      return true
    } else {
      return super.onMouseDown(event)
    }
  }

  onMouseMove (event: MouseEvent) : boolean {
    let worldPos = this.map.screenToWorldPos(new Sim.Point(event.offsetX, event.offsetY))
    if (this.isDrawing) {
      this.stop.x = worldPos.x
      this.stop.y = worldPos.y
      this.updateRoom()
      this.map.requestRedraw()
    } else {
      super.onMouseMove(event)
    }
    return false
  }

  onMouseUp (event: MouseEvent) : boolean {
    if (this.isDrawing) {
      // add the room to the map
      this.updateRoom()
      let minx = Math.min(this.start.x, this.stop.x)
      let miny = Math.min(this.start.y, this.stop.y)
      let maxx = Math.max(this.start.x, this.stop.x)
      let maxy = Math.max(this.start.y, this.stop.y)

      eventBus.$emit('/client/building/reveal', new Sim.Rectangle(minx, miny, maxx, maxy))
      this.roomActor.clearRooms()
      this.currentRoom = new B.Room()
      this.currentRoom.isVisible = true
      this.roomActor.addRoom(this.currentRoom)
    }
    super.onMouseUp(event)
    this.isDrawing = false
    return false
  }

  updateRoom () {
    this.currentRoom.position.x = (this.start.x + this.stop.x) / 2
    this.currentRoom.position.y = (this.start.y + this.stop.y) / 2
    this.currentRoom.size.x = Math.abs(this.stop.x - this.start.x) / 2
    this.currentRoom.size.y = Math.abs(this.stop.y - this.start.y) / 2
    this.roomActor.updateVertexData()
  }

  render (renderer: Renderer) {
    if (this.isDrawing && !this.isActorVisible) {
      this.isActorVisible = true
      renderer.addActor(this.roomActor, 4)
    }

    if (!this.isDrawing && this.isActorVisible) {
      this.isActorVisible = false
      renderer.removeActor(this.roomActor)
    }
  }
}
