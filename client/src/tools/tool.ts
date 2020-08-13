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

import WorldMap from '../components/WorldMap.vue'
import * as Sim from '../simulation/simulation'
import eventbus from '../eventbus'

import LineActor from '../rendering/lineactor'
import FontActor from '../rendering/fontactor'
import Renderer from '../rendering/renderer'
import RenderLayers from '../components/renderlayers'

export default class Tool {
  map: WorldMap

  isDragging: boolean = false
  isMovingToken: boolean = false

  fontActor: FontActor = new FontActor()
  lineActor: LineActor = new LineActor()

  shouldDrawText = false
  isDrawingText = false

  mouseDownX: number = 0
  mouseDownY: number = 0

  measureFromX: number = 0
  measureFromY: number = 0

  measureToX: number = 0
  measureToY: number = 0

  lastTouch: TouchEvent | undefined = undefined

  timeTouchStart: Map<number, Date> = new Map()

  constructor (map: WorldMap) {
    this.map = map
  }

  onMouseDown (event: MouseEvent) : boolean {
    // Wether this tool did something with the given event
    let consumeEvent: boolean = false
    let worldPos = this.map.screenToWorldPos(new Sim.Point(event.offsetX, event.offsetY))
    this.mouseDownX = event.offsetX
    this.mouseDownY = event.offsetY
    this.measureFromX = event.offsetX
    this.measureFromY = event.offsetY
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
    this.measureToX = event.offsetX
    this.measureToY = event.offsetY
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
    this.measureToX = event.offsetX
    this.measureToY = event.offsetY
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

      let d = Math.hypot(this.mouseDownX - event.offsetX, this.mouseDownY - event.offsetY)
      let a = Math.atan2(ewp.y - swp.y, ewp.x - swp.x)
      if (d < 10) {
        let token = this.map.getSelection()
        if (token !== undefined) {
          a = token.rotation
        } else {
          a = 0
        }
      }
      this.map.clientMoveSelectedTo(swp.x, swp.y, a)
    }

    this.isDragging = false
    this.measureToX = event.offsetX
    this.measureToY = event.offsetY
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
      renderer.addActor(this.fontActor, RenderLayers.TOOL)
      renderer.addActor(this.lineActor, RenderLayers.TOOL)
    }

    let t : Sim.Token | undefined = this.map.getSelection()
    if (t) {
      let text = ''
      let wmp = renderer.camera.screenToWorldSpace(new Sim.Point(this.measureToX, this.measureToY))
      if (this.isMovingToken) {
        let mdwp = this.map.screenToWorldPos(new Sim.Point(this.measureFromX, this.measureFromY))
        let a = Math.atan2(wmp.y - mdwp.y, wmp.x - mdwp.x)
        a = a / Math.PI * 180
        text = a.toFixed(2).toString()
        this.lineActor.setLine(mdwp.x, mdwp.y, wmp.x, wmp.y, renderer.camera.screenToWorldSpaceDist(0.6))
      } else {
        let d = wmp.distTo(new Sim.Point(t.displayX, t.displayY))
        text = d.toFixed(2).toString() + 'm'
        this.lineActor.setLine(t.displayX, t.displayY, wmp.x, wmp.y, renderer.camera.screenToWorldSpaceDist(0.6))
      }
      // Make the font 30 px high
      let scale = renderer.camera.screenToWorldSpaceDist(30)
      this.fontActor.setText(text)
      this.fontActor.setPosition(wmp.x, wmp.y)
      this.fontActor.setScale(scale, scale)
    }

    if (!this.shouldDrawText && this.isDrawingText) {
      this.isDrawingText = false
      renderer.removeActor(this.fontActor)
      renderer.removeActor(this.lineActor)
    }
  }

  onTouchStart (event: TouchEvent) {
    // Store the current time for every new touch.
    for (let i = 0; i < event.changedTouches.length; i++) {
      this.timeTouchStart.set(event.changedTouches[i].identifier, new Date())
    }
    // Check if this is the first finger or a subsequent one
    if (event.targetTouches.length === 1) {
      // Transform the screen to a world position
      let touch = event.targetTouches[0]
      let offset = this.getTouchOffset(touch)
      let worldPos = this.map.screenToWorldPos(new Sim.Point(offset.x, offset.y))

      // If we already have a token selected we are in the middle of a rotation.
      // Compute the tokens new angle and rotate it
      let selectedToken = this.map.getSelection()
      if (selectedToken !== undefined) {
        let a = Math.atan2(worldPos.y - selectedToken.y, worldPos.x - selectedToken.x)
        // Currently, there is no dedicated rotate command so we use a full move
        this.map.clientMoveSelectedTo(selectedToken.x, selectedToken.y, a)
        this.map.setSelectedToken(undefined)
      } else {
        // This is not a rotation, try to select a token at the touched position
        this.map.selectTokenAt(worldPos.x, worldPos.y)
      }
      this.shouldDrawText = this.map.hasSelection()
    } else if (event.targetTouches.length === 2 && this.map.hasSelection()) {
      // Cancel the current move
      this.map.setSelectedToken(undefined)
      this.shouldDrawText = this.map.hasSelection()
    }
    this.map.requestRedraw()
    this.lastTouch = event
  }

  onTouchEnd (event: TouchEvent) {
    if (event.targetTouches.length === 0) {
      // The last finger left the screen
      let now = new Date()
      // Acquire the time at which the touch started
      let startDate = this.timeTouchStart.get(event.changedTouches[0].identifier)
      if (startDate === undefined) {
        startDate = new Date()
        console.log('Tool.onTouchEnd: touch start time undefined')
      }
      // Check if we have a selection and the current action is not a tap.
      if (this.map.hasSelection() && now.getTime() - startDate.getTime() > 500) {
        // Move and deselect the token.
        let touch = event.changedTouches[0]
        let offset = this.getTouchOffset(touch)
        let worldPos = this.map.screenToWorldPos(new Sim.Point(offset.x, offset.y))
        this.map.clientMoveSelectedTo(worldPos.x, worldPos.y)
        this.map.setSelectedToken(undefined)
      }
      this.shouldDrawText = false
    }
    this.map.requestRedraw()
    this.lastTouch = event
    // Clean up the timeTouchStart map by removing the start times of all
    // removed fingers
    for (let i = 0; i < event.changedTouches.length; i++) {
      this.timeTouchStart.delete(event.changedTouches[i].identifier)
    }
  }

  onTouchMove (event: TouchEvent) {
    if (event.targetTouches.length === 1) {
      if (this.map.hasSelection()) {
        // Draw the measurement line to the current finger position
        let offset = this.getTouchOffset(event.targetTouches[0])
        this.measureToX = offset.x
        this.measureToY = offset.y
        this.map.requestRedraw()
      } else if (this.lastTouch !== undefined) {
        // Move the camera by the given finger motion
        let deltaX = event.targetTouches[0].clientX - this.lastTouch.targetTouches[0].clientX
        let deltaY = event.targetTouches[0].clientY - this.lastTouch.targetTouches[0].clientY
        this.map.moveByScreenDelta(deltaX, deltaY)
        this.map.requestRedraw()
      }
    } else if (event.targetTouches.length === 2) {
      if (this.lastTouch !== undefined && this.lastTouch.targetTouches.length === 2) {
        // Zoom the camera. Maintain the world space distance between the two
        // fingers for intuitive zooming.
        let lastD = this.getTwoFingerDistance(this.lastTouch)
        let d = this.getTwoFingerDistance(event)
        this.map.multiplyZoom(d / lastD)
        this.map.requestRedraw()
      }
    }

    this.lastTouch = event
  }

  onTouchCancel (event: TouchEvent) {
    // The touches were canceled. Clean up any state we store about the current
    // touches.
    this.map.setSelectedToken(undefined)
    this.shouldDrawText = this.map.hasSelection()
    this.timeTouchStart.clear()
  }

  getTwoFingerDistance (event: TouchEvent): number {
    if (event.targetTouches.length < 2) {
      throw new Error('Expected at least two fingers.')
    }
    let deltaX = event.targetTouches[0].clientX - event.targetTouches[1].clientX
    let deltaY = event.targetTouches[0].clientY - event.targetTouches[1].clientY
    return Math.hypot(deltaX, deltaY)
  }

  /**
   * @brief Returns the offset of the touch to its target element. In the case
   *        of a tool that is the distance in pixels from the top left corner
   *        of the map.
   */
  getTouchOffset (touch: Touch): Sim.Point {
    if (touch.target === null) {
      return new Sim.Point(touch.pageX, touch.pageY)
    } else {
      let e = touch.target as HTMLElement
      let r = e.getBoundingClientRect()
      return new Sim.Point(touch.clientX - r.left, touch.clientY - r.top)
    }
  }
}
