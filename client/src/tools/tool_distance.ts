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
import * as Sim from '../simulation/simulation'
import eventBus from '../eventbus'
import Renderer from '../rendering/renderer'
import LineActor from '../rendering/lineactor'
import FontActor from '../rendering/fontactor'
import RenderLayers from '../components/renderlayers'

export default class ToolDistance extends Tool {
  fontActor: FontActor = new FontActor()
  lineActor: LineActor = new LineActor()

  shouldDrawText = false
  isDrawingText = false

  measureFromX: number = 0
  measureFromY: number = 0

  measureToX: number = 0
  measureToY: number = 0

  onMouseDown (event: MouseEvent) : boolean {
    this.measureFromX = event.offsetX
    this.measureFromY = event.offsetY
    this.shouldDrawText = true
    return true
  }

  onMouseMove (event: MouseEvent) : boolean {
    if (this.shouldDrawText) {
      this.measureToX = event.offsetX
      this.measureToY = event.offsetY
      this.map.requestRedraw()
      return true
    }
    return false
  }

  onMouseUp (event: MouseEvent) : boolean {
    if (this.shouldDrawText) {
      this.shouldDrawText = false
      this.map.requestRedraw()
    }
    return false
  }

  render (renderer: Renderer) {
    if (this.shouldDrawText && !this.isDrawingText) {
      this.isDrawingText = true
      renderer.addActor(this.fontActor, RenderLayers.TOOL)
      renderer.addActor(this.lineActor, RenderLayers.TOOL)
    }

    if (this.shouldDrawText) {
      let measureToWorld = renderer.camera.screenToWorldSpace(new Sim.Point(this.measureToX, this.measureToY))
      let measureFromWorld = renderer.camera.screenToWorldSpace(new Sim.Point(this.measureFromX, this.measureFromY))

      let d = measureFromWorld.distTo(measureToWorld)
      let text = d.toFixed(2).toString() + 'm'
      this.lineActor.setLine(measureFromWorld.x, measureFromWorld.y, measureToWorld.x, measureToWorld.y, renderer.camera.screenToWorldSpaceDist(0.6))

      // Make the font 30 px high
      let scale = renderer.camera.screenToWorldSpaceDist(30)
      this.fontActor.setText(text)
      this.fontActor.setPosition((measureToWorld.x + measureFromWorld.x) / 2, (measureFromWorld.y + measureToWorld.y) / 2)
      this.fontActor.setScale(scale, scale)
    }

    if (!this.shouldDrawText && this.isDrawingText) {
      this.isDrawingText = false
      renderer.removeActor(this.fontActor)
      renderer.removeActor(this.lineActor)
    }
  }
}
