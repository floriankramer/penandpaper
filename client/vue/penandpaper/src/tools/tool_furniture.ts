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

import FurnitureActor from '../rendering/furnitureactor'
import FontActor from '../rendering/fontactor'
import RenderLayers from '../components/renderlayers'

export default class ToolFurniture extends Tool {
  isDrawing: boolean = false
  isRotating: boolean = false
  isCloning: boolean = false

  start: Sim.Point = new Sim.Point(0, 0)
  stop: Sim.Point = new Sim.Point(0, 0)

  accuracy: number = 20

  currentFurniture: B.Furniture = new B.Furniture()

  rotatedFurniture: B.Furniture | undefined = undefined
  rotationPivot: Sim.Point = new Sim.Point(0, 0)

  cloningOffset: Sim.Point = new Sim.Point(0, 0)

  furnitureActor = new FurnitureActor()
  fontActor = new FontActor()
  isActorVisible: boolean = false

  constructor (map: Map) {
    super(map)
    this.currentFurniture.isVisible = true
    this.furnitureActor.addFurniture(this.currentFurniture)
  }

  onMouseDown (event: MouseEvent) : boolean {
    let worldPos = this.map.screenToWorldPos(new Sim.Point(event.offsetX, event.offsetY))
    if (event.ctrlKey && event.shiftKey) {
      let f : B.Furniture | undefined = this.map.getFurnitureAt(worldPos)
      if (f !== undefined) {
        this.currentFurniture.modify(f)
        this.isCloning = true
        let c = this.currentFurniture.position
        this.cloningOffset.x = c.x - worldPos.x
        this.cloningOffset.y = c.y - worldPos.y
      }
    } else if (event.ctrlKey) {
      if (event.button === 2) {
        eventBus.$emit('/client/building/furniture/delete', worldPos)
      } else {
        this.start.x = Math.round(worldPos.x * this.accuracy) / this.accuracy
        this.start.y = Math.round(worldPos.y * this.accuracy) / this.accuracy
        this.stop.x = this.start.x
        this.stop.y = this.start.y
        this.isDrawing = true
      }
      return true
    } else if (event.altKey) {
      // Check if there is a piece of furniture we can rotate
      this.rotatedFurniture = this.map.getFurnitureAt(worldPos)
      if (this.rotatedFurniture !== undefined) {
        if (event.button === 2) {
          this.rotatedFurniture.rotation = 0
          this.map.requestRedraw()
          this.rotatedFurniture = undefined
        } else {
          this.isRotating = true
          this.rotationPivot = this.rotatedFurniture.position
        }
      }
    } else {
      return super.onMouseDown(event)
    }
    return true
  }

  onMouseMove (event: MouseEvent) : boolean {
    let worldPos = this.map.screenToWorldPos(new Sim.Point(event.offsetX, event.offsetY))
    if (this.isDrawing) {
      this.stop.x = Math.round(worldPos.x * this.accuracy) / this.accuracy
      this.stop.y = Math.round(worldPos.y * this.accuracy) / this.accuracy
      this.updateFurniture()
      this.map.requestRedraw()
    } else if (this.isRotating && this.rotatedFurniture !== undefined) {
      this.rotatedFurniture.rotation = Math.atan2(worldPos.y - this.rotationPivot.y, worldPos.x - this.rotationPivot.x)
      this.map.requestRedraw()
    } else if (this.isCloning) {
      let p = new Sim.Point(worldPos.x + this.cloningOffset.x, worldPos.y + this.cloningOffset.y)
      p.x = Math.round(p.x * this.accuracy) / this.accuracy
      p.y = Math.round(p.y * this.accuracy) / this.accuracy
      this.currentFurniture.setCenter(p)
      this.map.requestRedraw()
    } else {
      return super.onMouseMove(event)
    }
    return false
  }

  onMouseUp (event: MouseEvent) : boolean {
    if (this.isDrawing) {
      // add the room to the map
      this.updateFurniture()
      if (this.currentFurniture.width() > 0.1 && this.currentFurniture.height() > 0.1) {
        eventBus.$emit('/client/building/furniture/create', this.currentFurniture)
      }
      this.currentFurniture = new B.Furniture()
      this.currentFurniture.isVisible = true
      this.furnitureActor.clearFurniture()
      this.furnitureActor.addFurniture(this.currentFurniture)
    } else if (this.isRotating) {
      this.rotatedFurniture = undefined
    } else if (this.isCloning) {
      eventBus.$emit('/client/building/furniture/create', this.currentFurniture)
      this.currentFurniture = new B.Furniture()
      this.currentFurniture.isVisible = true
      this.furnitureActor.clearFurniture()
      this.furnitureActor.addFurniture(this.currentFurniture)
    }
    super.onMouseUp(event)
    this.isDrawing = false
    this.isRotating = false
    this.isCloning = false
    return false
  }

  updateFurniture () {
    this.currentFurniture.position.x = (this.start.x + this.stop.x) / 2
    this.currentFurniture.position.y = (this.start.y + this.stop.y) / 2
    this.currentFurniture.size.x = Math.abs(this.stop.x - this.start.x) / 2
    this.currentFurniture.size.y = Math.abs(this.stop.y - this.start.y) / 2
    this.furnitureActor.updateVertexData()
  }

  render (renderer: Renderer) {
    if ((this.isDrawing || this.isCloning) && !this.isActorVisible) {
      this.isActorVisible = true
      renderer.addActor(this.furnitureActor, RenderLayers.TOOL)
      renderer.addActor(this.fontActor, RenderLayers.TOOL)
    }

    let t = this.currentFurniture.size.x.toFixed(2) + 'm x ' + this.currentFurniture.size.y.toFixed(2) + 'm'
    this.fontActor.setText(t)
    this.fontActor.setPosition(this.currentFurniture.position.x + this.currentFurniture.size.x, this.currentFurniture.position.y - this.currentFurniture.size.y)

    if (!(this.isDrawing || this.isCloning) && this.isActorVisible) {
      this.isActorVisible = false
      renderer.removeActor(this.furnitureActor)
      renderer.removeActor(this.fontActor)
    }
  }
}
