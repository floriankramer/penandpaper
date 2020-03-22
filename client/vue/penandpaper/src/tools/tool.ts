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

import Map from '../components/Map.vue'
import * as Sim from '../simulation/simulation'
import eventbus from '../eventbus'

import LineActor from '../rendering/lineactor'
import FontActor from '../rendering/fontactor'
import Renderer from '../rendering/renderer'

export default class Tool {
  map: Map

  isDragging: boolean = false
  isMovingToken: boolean = false

  fontActor: FontActor = new FontActor()
  lineActor: LineActor = new LineActor()

  shouldDrawText = false
  isDrawingText = false

  mouseDownX: number = 0
  mouseDownY: number = 0

  constructor (map: Map) {
    this.map = map
  }

  onMouseDown (event: MouseEvent) : boolean {
    // Wether this tool did something with the given event
    let consumeEvent: boolean = false
    let worldPos = this.map.screenToWorldPos(new Sim.Point(event.offsetX, event.offsetY))
    this.mouseDownX = event.offsetX
    this.mouseDownY = event.offsetY
    if (event.button === 0) {
      if (event.shiftKey) {
        if (this.map.hasSelection()) {
          this.isMovingToken = true
          consumeEvent = true
        } else {
          eventbus.$emit('/client/building/door/toggle', worldPos)
          consumeEvent = true
        }
      } else {
        let clickedToken: boolean = this.map.selectTokenAt(worldPos.x, worldPos.y)
        if (!clickedToken) {
          this.isDragging = true
        }
        consumeEvent = true
      }
    } else if (event.button === 1) {
      this.map.resetCamera()
      consumeEvent = true
    } else if (event.button === 2) {
      this.isMovingToken = true
      consumeEvent = true
    }
    this.map.setLastMousePos(event.offsetX, event.offsetY)
    this.shouldDrawText = this.map.hasSelection()
    return consumeEvent
  }

  onMouseMove (event: MouseEvent) : boolean {
    // Wether this tool did something with the given event
    let consumeEvent: boolean = false
    let doRender = false
    if (this.isDragging) {
      consumeEvent = true
      this.map.moveByScreenDelta(event.movementX, event.movementY)
      doRender = true
    }
    this.map.setLastMousePos(event.offsetX, event.offsetY)
    if (this.map.hasSelection()) {
      // Redraw for the distance measurement
      doRender = true
    }
    if (doRender) {
      this.map.requestRedraw()
    }
    return consumeEvent
  }

  onMouseUp (event: MouseEvent) : boolean {
    // Wether this tool did something with the given event
    let consumeEvent: boolean = false
    if (this.isMovingToken) {
      this.isMovingToken = false
      consumeEvent = true
      let swp = this.map.screenToWorldPos(new Sim.Point(this.mouseDownX, this.mouseDownY))
      let ewp = this.map.screenToWorldPos(new Sim.Point(event.offsetX, event.offsetY))
      let a = Math.atan2(ewp.y - swp.y, ewp.x - swp.x)
      this.map.clientMoveSelectedTo(swp.x, swp.y, a)
    }

    this.isDragging = false
    this.map.setLastMousePos(event.offsetX, event.offsetY)
    this.map.requestRedraw()
    return consumeEvent
  }

  onKeyDown (event: KeyboardEvent) : boolean {
    // Wether this tool did something with the given event
    let consumeEvent: boolean = false
    if (event.key === 'Delete') {
      if (this.map.hasSelection()) {
        this.map.clientDeleteSelectedToken()
        consumeEvent = true
      }
    } else if (event.key === 'f') {
      if (this.map.hasSelection()) {
        this.map.clientToggleFoeSelectedToken()
        consumeEvent = true
      }
    }
    return consumeEvent
  }

  render (renderer: Renderer) {
    if (this.shouldDrawText && !this.isDrawingText) {
      this.isDrawingText = true
      renderer.addActor(this.fontActor, 4)
      renderer.addActor(this.lineActor, 4)
    }

    if (this.map.hasSelection()) {
      let t : Sim.Token | undefined = this.map.getSelection()
      if (t) {
        let mp : Sim.Point = this.map.getLastMousePos()
        let wmp = renderer.camera.screenToWorldSpace(mp)
        let d = wmp.distTo(new Sim.Point(t.x, t.y))

        // Make the font 30 px high
        let scale = renderer.camera.screenToWorldSpaceDist(30)
        this.fontActor.setText(d.toFixed(2).toString() + 'm')
        this.fontActor.setPosition(wmp.x, wmp.y)
        this.fontActor.setScale(scale, scale)

        this.lineActor.setLine(t.displayX, t.displayY, wmp.x, wmp.y, renderer.camera.screenToWorldSpaceDist(0.6))
      }
    }

    if (!this.shouldDrawText && this.isDrawingText) {
      this.isDrawingText = false
      renderer.removeActor(this.fontActor)
      renderer.removeActor(this.lineActor)
    }
  }
}
