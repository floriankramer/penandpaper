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
import Renderer from '../rendering/renderer'

export default class ToolToken extends Tool {
  isDrawing: boolean = false
  lastLineStop: Sim.Point = new Sim.Point(0, 0)
  lastLineStopScreen: Sim.Point = new Sim.Point(0, 0)
  currentLineStop: Sim.Point = new Sim.Point(0, 0)

  onMouseDown (event: MouseEvent) : boolean {
    if (event.ctrlKey) {
      let worldPos = this.map.screenToWorldPos(new Sim.Point(event.offsetX, event.offsetY))
      this.lastLineStop.x = worldPos.x
      this.lastLineStop.y = worldPos.y
      this.lastLineStopScreen.x = event.offsetX
      this.lastLineStopScreen.y = event.offsetY
      this.currentLineStop.x = worldPos.x
      this.currentLineStop.y = worldPos.y
      this.isDrawing = true
      return true
    } else {
      return super.onMouseDown(event)
    }
  }

  onMouseMove (event: MouseEvent) : boolean {
    if (this.isDrawing) {
      let worldPos = this.map.screenToWorldPos(new Sim.Point(event.offsetX, event.offsetY))
      this.currentLineStop.x = worldPos.x
      this.currentLineStop.y = worldPos.y
      if (Math.hypot(this.lastLineStopScreen.x - event.offsetX, this.lastLineStopScreen.y - event.offsetY) > this.map.$el.clientHeight / 20) {
        // create a new line
        let line = new Sim.Line()
        line.start.x = this.lastLineStop.x
        line.start.y = this.lastLineStop.y
        line.stop.x = worldPos.x
        line.stop.y = worldPos.y
        eventBus.$emit('/client/line/create', line)
        this.lastLineStop = worldPos
        this.lastLineStopScreen.x = event.offsetX
        this.lastLineStopScreen.y = event.offsetY
      }
      this.map.requestRedraw()
    } else {
      super.onMouseMove(event)
    }
    return false
  }

  onMouseUp (event: MouseEvent) : boolean {
    if (this.isDrawing) {
      // create a new line
      let worldPos = this.map.screenToWorldPos(new Sim.Point(event.offsetX, event.offsetY))
      let line = new Sim.Line()
      line.start.x = this.lastLineStop.x
      line.start.y = this.lastLineStop.y
      line.stop.x = worldPos.x
      line.stop.y = worldPos.y
      eventBus.$emit('/client/line/create', line)
      this.lastLineStop = worldPos
    }
    super.onMouseUp(event)
    this.isDrawing = false
    return false
  }

  render (renderer: Renderer) {
    // if (this.isDrawing) {
    //   // Draw the lines
    //   ctx.strokeStyle = '#FFFFEE'
    //   ctx.lineWidth = this.map.computeLineWidth()
    //   ctx.beginPath()
    //   ctx.moveTo(this.lastLineStop.x, this.lastLineStop.y)
    //   ctx.lineTo(this.currentLineStop.x, this.currentLineStop.y)
    //   ctx.stroke()
    // }
  }
}
