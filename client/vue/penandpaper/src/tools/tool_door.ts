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

export default class ToolDoor extends Tool {
  isDrawing: boolean = false
  start: Sim.Point = new Sim.Point(0, 0)
  stop: Sim.Point = new Sim.Point(0, 0)

  accuracy: number = 4

  currentDoor: B.Door = new B.Door()

  standardDoorWidth: number = 1.2
  useStandardDoor: boolean = false

  onMouseDown (event: MouseEvent) : boolean {
    if (event.ctrlKey) {
      let worldPos = this.map.screenToWorldPos(new Sim.Point(event.offsetX, event.offsetY))
      if (event.button === 2) {
        this.map.removeDoorAt(worldPos.x, worldPos.y)
        this.map.requestRedraw()
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
        this.map.addDoor(this.currentDoor)
      }
      this.currentDoor = new B.Door()
    }
    super.onMouseUp(event)
    this.isDrawing = false
    return false
  }

  updateDoor () {
    let delta = new Sim.Point(this.stop.x - this.start.x, this.stop.y - this.start.y)
    this.currentDoor.facing = delta.normalized()
    this.currentDoor.facing.toCardinalDirection()
    if (this.useStandardDoor) {
      this.currentDoor.width = this.standardDoorWidth
    } else {
      this.currentDoor.width = delta.length()
    }

    if (Math.abs(this.currentDoor.facing.x) < 0.5) {
      // snap to y
      this.currentDoor.position.x = this.start.x
      this.currentDoor.position.y = Math.round(this.start.y * this.accuracy) / this.accuracy
    } else {
      // snap to x
      this.currentDoor.position.x = Math.round(this.start.x * this.accuracy) / this.accuracy
      this.currentDoor.position.y = this.start.y
    }
  }

  render (ctx: CanvasRenderingContext2D) {
    if (this.isDrawing) {
      ctx.lineWidth = this.map.computeLineWidth()
      this.currentDoor.render(ctx)

      let text = this.currentDoor.width.toFixed(1) + 'm'
      ctx.fillStyle = '#FFFFFF'
      this.map.setupScreenSpaceFont(ctx)
      let screenSpacePos = this.map.worldToScreenPos(new Sim.Point(this.currentDoor.position.x, this.currentDoor.position.y))
      let transform = ctx.getTransform()
      ctx.resetTransform()
      ctx.fillText(text, screenSpacePos.x + 10, screenSpacePos.y)
      ctx.setTransform(transform)
    }
  }
}
