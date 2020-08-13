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
import WorldMap from '../components/WorldMap.vue'
import * as Sim from '../simulation/simulation'
import eventBus from '../eventbus'
import * as B from '../simulation/building'
import Renderer from '../rendering/renderer'

import DoorActor from '../rendering/dooractor'
import RenderLayers from '../components/renderlayers'

export default class ToolDoor extends Tool {
  isDrawing: boolean = false
  start: Sim.Point = new Sim.Point(0, 0)
  stop: Sim.Point = new Sim.Point(0, 0)

  accuracy: number = 4

  currentDoor: B.Door = new B.Door()

  standardDoorWidth: number = 1.2
  useStandardDoor: boolean = false

  doorActor = new DoorActor()
  isActorVisible: boolean = false

  constructor (map: WorldMap) {
    super(map)
    this.currentDoor.isVisible = true
    this.doorActor.addDoor(this.currentDoor)
  }

  onMouseDown (event: MouseEvent) : boolean {
    if (event.ctrlKey) {
      let worldPos = this.map.screenToWorldPos(new Sim.Point(event.offsetX, event.offsetY))
      if (event.button === 2) {
        eventBus.$emit('/client/building/door/delete', worldPos)
      } else {
        this.start.x = worldPos.x
        this.start.y = worldPos.y
        this.stop.x = this.start.x
        this.stop.y = this.start.y
        this.isDrawing = true
      }
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
      this.useStandardDoor = event.altKey
      // Draw the lines
      this.updateDoor()
      this.map.requestRedraw()
    } else {
      super.onMouseMove(event)
    }
    return false
  }

  onMouseUp (event: MouseEvent) : boolean {
    if (this.isDrawing) {
      // create a new door
      this.updateDoor()
      if (this.currentDoor.width > 0.1) {
        eventBus.$emit('/client/building/door/create', this.currentDoor)
      }
      this.currentDoor = new B.Door()
      this.currentDoor.isVisible = true
      this.doorActor.clearDoors()
      this.doorActor.addDoor(this.currentDoor)
    }
    super.onMouseUp(event)
    this.isDrawing = false
    return false
  }

  updateDoor () {
    let delta = this.stop.minus(this.start)
    let step = Math.PI / 8
    this.currentDoor.rotation = Math.round(Math.atan2(delta.y, delta.x) / step) * step
    if (this.useStandardDoor) {
      this.currentDoor.width = this.standardDoorWidth
    } else {
      this.currentDoor.width = delta.length()
    }

    let facingUp = this.currentDoor.rotation > Math.PI / 4 && this.currentDoor.rotation < 3 * Math.PI / 4
    let facingDown = this.currentDoor.rotation > -Math.PI / 4 && this.currentDoor.rotation < 3 * -Math.PI / 4

    if (facingUp || facingDown) {
      // snap to y
      this.currentDoor.position.x = this.start.x
      this.currentDoor.position.y = Math.round(this.start.y * this.accuracy) / this.accuracy
    } else {
      // snap to x
      this.currentDoor.position.x = Math.round(this.start.x * this.accuracy) / this.accuracy
      this.currentDoor.position.y = this.start.y
    }
    this.doorActor.updateVertexData()
  }

  render (renderer: Renderer) {
    if (this.isDrawing && !this.isActorVisible) {
      this.isActorVisible = true
      renderer.addActor(this.doorActor, RenderLayers.TOOL)
    }

    if (!this.isDrawing && this.isActorVisible) {
      this.isActorVisible = false
      renderer.removeActor(this.doorActor)
    }
  }
}
