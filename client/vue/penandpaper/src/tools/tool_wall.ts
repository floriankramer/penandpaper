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

import WallActor from '../rendering/wallactor'
import FontActor from '../rendering/fontactor'

export default class ToolWall extends Tool {
  isDrawing: boolean = false
  start: Sim.Point = new Sim.Point(0, 0)
  stop: Sim.Point = new Sim.Point(0, 0)

  accuracy: number = 4

  currentWall: B.Wall = new B.Wall()

  wallActor: WallActor = new WallActor()
  fontActor: FontActor = new FontActor()
  isActorVisible: boolean = false

  constructor (map: Map) {
    super(map)
    this.currentWall.isVisible = true
    this.wallActor.addWall(this.currentWall)
  }

  onMouseDown (event: MouseEvent) : boolean {
    if (event.ctrlKey) {
      let worldPos = this.map.screenToWorldPos(new Sim.Point(event.offsetX, event.offsetY))
      if (event.button === 2) {
        eventBus.$emit('/client/building/wall/delete', worldPos)
      } else {
        this.start.x = Math.round(worldPos.x * this.accuracy) / this.accuracy
        this.start.y = Math.round(worldPos.y * this.accuracy) / this.accuracy
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
      this.stop.x = Math.round(worldPos.x * this.accuracy) / this.accuracy
      this.stop.y = Math.round(worldPos.y * this.accuracy) / this.accuracy
      this.updateWall()
      this.map.requestRedraw()
    } else {
      super.onMouseMove(event)
      this.start.x = Math.round(worldPos.x * this.accuracy) / this.accuracy
      this.start.y = Math.round(worldPos.y * this.accuracy) / this.accuracy
      this.stop.x = this.start.x + 0.4
      this.stop.y = this.start.y + 0.4
      this.updateWall()
      this.map.requestRedraw()
    }
    return false
  }

  onMouseUp (event: MouseEvent) : boolean {
    if (this.isDrawing) {
      // add the Wall to the map
      this.updateWall()
      if (this.currentWall.length() > 0.1) {
        eventBus.$emit('/client/building/wall/create', this.currentWall)
      }
      this.wallActor.clearWalls()
      this.currentWall = new B.Wall()
      this.currentWall.isVisible = true
      this.wallActor.addWall(this.currentWall)
    }
    super.onMouseUp(event)
    this.isDrawing = false
    return false
  }

  updateWall () {
    this.currentWall.start.x = this.start.x
    this.currentWall.start.y = this.start.y
    this.currentWall.end.x = this.stop.x
    this.currentWall.end.y = this.stop.y
    this.wallActor.updateVertexData()
  }

  render (renderer: Renderer) {
    if (this.isDrawing && !this.isActorVisible) {
      this.isActorVisible = true
      renderer.addActor(this.wallActor, 4)
      renderer.addActor(this.fontActor, 4)
    }

    let t = this.currentWall.length().toFixed(2) + 'm'
    this.fontActor.setText(t)
    this.fontActor.setPosition(this.currentWall.end.x, this.currentWall.end.y)

    if (!this.isDrawing && this.isActorVisible) {
      this.isActorVisible = false
      renderer.removeActor(this.wallActor)
      renderer.removeActor(this.fontActor)
    }
  }
}
