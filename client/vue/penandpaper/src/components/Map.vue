<!--
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
-->

<template>
  <div>
    <canvas v-on:mousedown="onMouseDown" v-on:mouseup="onMouseUp"
            v-on:mousemove="onMouseMove" v-on:wheel="onMouseWheel"
            v-on:contextmenu.prevent v-on:keydown="onKeyDown"
            tabindex="0"/>
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator'
import Server, { ServerState } from './server'
import eventBus from '../eventbus'
import * as Sim from '../simulation/simulation'
import Tool from '../tools/tool'
import ToolToken from '../tools/tool_token'
import ToolLine from '../tools/tool_line'
import ToolRoom from '../tools/tool_room'
import ToolDoor from '../tools/tool_door'
import ToolFurniture from '../tools/tool_furniture'
import Renderer from '../rendering/renderer'
import * as B from '../simulation/building'

enum MouseAction {
  NONE,
  DRAG,
  CREATE_LINE
}

@Component
export default class Map extends Vue {
  tokens: Sim.Token[] = []
  movingTokens: Sim.Token[] = []
  lines: Sim.Line[] = []

  canvas?: HTMLCanvasElement
  ctx: WebGLRenderingContext | null = null
  renderer: Renderer = new Renderer()

  mouseAction: MouseAction = MouseAction.NONE

  lastMouseX: number = 0
  lastMouseY: number = 0

  zoom: number = 1
  offx: number = 0
  offy: number = 0

  pixelPerMeter: number = 1

  selected?: Sim.Token

  renderQueued: boolean = false
  lastRender: number = 0

  tool: Tool = new Tool(this)

  currentBuilding?: B.Building = undefined

  constructor () {
    super()

    eventBus.$on('/server/token/create', (data: Sim.Token) => { this.onNewToken(data) })
    eventBus.$on('/server/token/clear', () => { this.clearTokens() })
    eventBus.$on('/server/token/delete', (data: Sim.Token) => { this.onServerDeleteToken(data) })
    eventBus.$on('/server/token/move', (data: Sim.TokenMoveOrder) => { this.onServerMoveToken(data) })
    eventBus.$on('/server/token/toggle_foe', (data: Sim.Token) => { this.onServerToggleFoe(data) })
    eventBus.$on('/server/line/create', (data: Sim.Line) => { this.onServerCreateLine(data) })
    eventBus.$on('/server/line/clear', () => { this.onServerClearLines() })
    eventBus.$on('/server/state', (data: ServerState) => { this.onServerState(data) })
    eventBus.$on('/server/building/set', (data: B.Building) => { this.onServerSetBuilding(data) })
    eventBus.$on('/server/building/toggle_door', (data: number[]) => { this.onServerToggleDoor(data) })

    eventBus.$on('/client/building/save', () => { this.onClientSaveBuilding() })
    eventBus.$on('/client/building/load', (file: File) => { this.onClientLoadBuilding(file) })

    eventBus.$on('/tools/select_tool', (data: string) => { this.onToolSelected(data) })
  }

  requestRedraw () {
    requestAnimationFrame(this.renderMap)
  }

  renderMap () {
    let now = new Date().getTime()

    if (this.ctx !== null && this.canvas) {
      // Assume that without zoom 10 meters are visible vertically
      // Determine the visible area
      // let visibleHeight: number = 10 / this.zoom
      // let visibleWidth: number = visibleHeight * (this.canvas.width / this.canvas.height)

      // let visible: Sim.Rectangle = new Sim.Rectangle()
      // visible.minx = this.offx - visibleWidth / 2
      // visible.maxx = this.offx + visibleWidth / 2
      // visible.miny = this.offy - visibleHeight / 2
      // visible.maxy = this.offy + visibleHeight / 2

      this.renderer.beginFrame()
      this.renderer.drawFrame()
      this.renderer.endFrame()

      // // Clear the canvas
      // ctx.resetTransform()

      // ctx.fillStyle = '#333333'
      // ctx.fillRect(0, 0, this.canvas.width, this.canvas.height)

      // // Make 0,0 the center
      // ctx.translate(this.canvas.width / 2, this.canvas.height / 2)
      // // Apply the base scale
      // ctx.scale(this.pixelPerMeter, this.pixelPerMeter)

      // // apply the offset
      // ctx.scale(this.zoom, this.zoom)
      // ctx.translate(-this.offx, -this.offy)

      // // Draw a 5x5 m grid centered on 0, 0 (0, 0 is a crossign point of two grid lines)
      // let gridXMin = Math.floor(visible.minx / 5) * 5
      // let gridYMin = Math.floor(visible.miny / 5) * 5
      // let gridXMax = Math.ceil(visible.maxx / 5) * 5
      // let gridYMax = Math.ceil(visible.maxy / 5) * 5

      // ctx.beginPath()
      // for (let x : number = gridXMin; x <= gridXMax; x += 5) {
      //   ctx.moveTo(x, visible.miny)
      //   ctx.lineTo(x, visible.maxy)
      // }
      // for (let y : number = gridYMin; y <= gridYMax; y += 5) {
      //   ctx.moveTo(visible.minx, y)
      //   ctx.lineTo(visible.maxx, y)
      // }
      // ctx.lineWidth = 1 / this.pixelPerMeter / this.zoom
      // ctx.strokeStyle = '#4c4c4c'
      // ctx.stroke()

      // // Draw the current building
      // if (this.currentBuilding !== undefined) {
      //   ctx.lineWidth = this.computeLineWidth()
      //   this.currentBuilding.render(ctx, this.$store.state.permissions > 0)
      // }

      // // Draw the lines
      // ctx.strokeStyle = '#FFFFEE'
      // ctx.lineWidth = this.computeLineWidth()
      // ctx.beginPath()
      // for (let line of this.lines) {
      //   ctx.moveTo(line.start.x, line.start.y)
      //   ctx.lineTo(line.stop.x, line.stop.y)
      // }
      // ctx.stroke()

      // // Draw the tokens
      // for (let token of this.tokens) {
      //   ctx.beginPath()
      //   ctx.arc(token.displayX, token.displayY, token.radius, 0, 2 * Math.PI)
      //   ctx.fillStyle = token.color.toHex()
      //   ctx.fill()

      //   if (this.selected !== undefined && this.selected.id === token.id) {
      //     if (token.isFoe) {
      //       ctx.strokeStyle = '#FF0000'
      //       ctx.lineWidth = 0.04
      //       ctx.stroke()
      //     } else {
      //       ctx.strokeStyle = '#FFFFFF'
      //       ctx.lineWidth = 0.03
      //       ctx.stroke()
      //     }
      //   } else if (token.isFoe) {
      //     ctx.strokeStyle = '#FF0000'
      //     ctx.lineWidth = 0.05
      //     ctx.stroke()
      //   }
      // }

      // if (this.selected !== undefined) {
      //   // Draw a line from the selected to the cursor
      //   let worldPos = this.screenToWorldPos(new Sim.Point(this.lastMouseX, this.lastMouseY))
      //   ctx.strokeStyle = '#696969'
      //   ctx.lineWidth = 1.5 / this.pixelPerMeter / this.zoom

      //   ctx.beginPath()
      //   ctx.moveTo(this.selected.displayX, this.selected.displayY)
      //   ctx.lineTo(worldPos.x, worldPos.y)
      //   ctx.stroke()

      //   // Draw a distance at the cursor
      //   let dist = Math.hypot(worldPos.x - this.selected.displayX, worldPos.y - this.selected.displayY)
      //   ctx.fillStyle = ctx.strokeStyle
      //   this.setupScreenSpaceFont(ctx)
      //   ctx.fillStyle = '#AAAAAA'
      //   let transform = ctx.getTransform()
      //   ctx.resetTransform()
      //   ctx.fillText(dist.toFixed(2).toString() + 'm', this.lastMouseX + 20, this.lastMouseY - 20)
      //   ctx.setTransform(transform)
      // }
      // this.tool.render(ctx)
    }
    let end: number = Date.now()
    console.log(end - now)
  }

  setupScreenSpaceFont (ctx: CanvasRenderingContext2D) {
    ctx.font = '30px sans-serif'
  }

  setupWorldSpaceFont (ctx: CanvasRenderingContext2D) {
    let size = 30 / this.zoom
    ctx.font = size.toString() + 'px sans-serif'
  }

  computeLineWidth () : number {
    return 2 / this.pixelPerMeter / this.zoom
  }

  screenToWorldPos (point: Sim.Point) : Sim.Point {
    return new Sim.Point((point.x - this.$el.clientWidth / 2) / this.pixelPerMeter / this.zoom + this.offx,
      (point.y - this.$el.clientHeight / 2) / this.pixelPerMeter / this.zoom + this.offy)
  }

  worldToScreenPos (point: Sim.Point) : Sim.Point {
    return new Sim.Point((point.x - this.offx) * this.zoom * this.pixelPerMeter + this.$el.clientWidth / 2,
      (point.y - this.offy) * this.zoom * this.pixelPerMeter + this.$el.clientHeight / 2)
  }

  moveByScreenDelta (dx: number, dy: number) {
    this.offx -= dx / this.pixelPerMeter / this.zoom
    this.offy -= dy / this.pixelPerMeter / this.zoom
    if (this.canvas) {
      this.renderer.camera.pan(dx * 2 / this.canvas.height, dy * 2 / this.canvas.height)
    }
  }

  hasSelection () : boolean {
    return this.selected !== undefined
  }

  resetCamera () {
    this.offx = 0
    this.offy = 0
    this.zoom = 1
  }

  setLastMousePos (sx: number, sy: number) {
    this.lastMouseX = sx
    this.lastMouseY = sy
  }

  addRoom (room: B.Room) {
    if (this.currentBuilding === undefined) {
      this.currentBuilding = new B.Building()
    }
    this.currentBuilding.addRoom(room)
    this.requestRedraw()
  }

  addFurniture (f: B.Furniture) {
    if (this.currentBuilding === undefined) {
      this.currentBuilding = new B.Building()
    }
    this.currentBuilding.addFurniture(f)
    this.requestRedraw()
  }

  addDoor (door: B.Door) {
    if (this.currentBuilding === undefined) {
      this.currentBuilding = new B.Building()
    }
    this.currentBuilding.addDoor(door)
    this.requestRedraw()
  }

  removeRoomAt (wx: number, wy: number) {
    if (this.currentBuilding !== undefined) {
      this.currentBuilding.removeRoomAt(wx, wy)
    }
  }

  removeFurnitureAt (pos: Sim.Point) {
    if (this.currentBuilding !== undefined) {
      this.currentBuilding.removeFurnitureAt(pos)
    }
  }

  getFurnitureAt (pos: Sim.Point): B.Furniture | undefined {
    if (this.currentBuilding !== undefined) {
      return this.currentBuilding.getFurnitureAt(pos)
    }
    return undefined
  }

  removeDoorAt (wx: number, wy: number) {
    if (this.currentBuilding !== undefined) {
      this.currentBuilding.removeDoorAt(wx, wy)
    }
  }

  canToggleDoorAt (wx: number, wy: number) : boolean {
    if (this.$store.state.permissions === 1) {
      if (this.currentBuilding !== undefined) {
        return this.currentBuilding.isDoorAt(wx, wy)
      }
      return false
    }
    return false
  }

  getDoorsAt (wx: number, wy: number) : B.Door[] {
    if (this.currentBuilding !== undefined) {
      return this.currentBuilding.getDoorsAt(wx, wy)
    }
    return []
  }

  getRoomAt (pos: Sim.Point) : B.Room | undefined {
    if (this.currentBuilding !== undefined) {
      return this.currentBuilding.getRoomAt(pos)
    }
    return undefined
  }

  onServerToggleDoor (doors: number[]) {
    if (this.currentBuilding !== undefined) {
      // TODO: synchronize
      let b = this.currentBuilding.toggleDoors(doors)
      this.requestRedraw()
      return b
    }
    return false
  }

  revealRoomsAt (wpos: Sim.Point) {
    if (this.currentBuilding !== undefined) {
      this.currentBuilding.revealRoomsAt(wpos)
      eventBus.$emit('/client/building/set', this.currentBuilding)
    }
  }

  /**
   * @param wx The x coordinate in world space
   * @param wy The y coordinate in world space
   * @returns True if a new token was selected
   */
  selectTokenAt (wx: number, wy: number) : boolean {
    for (let token of this.tokens) {
      if (Math.hypot(token.x - wx, token.y - wy) < token.radius) {
        this.selected = token
        return true
      }
    }
    this.selected = undefined
    return false
  }

  onKeyDown (event: KeyboardEvent) {
    this.tool.onKeyDown(event)
  }

  onMouseDown (event: MouseEvent) {
    this.tool.onMouseDown(event)
  }

  clientMoveSelectedTo (x: number, y: number) {
    if (this.selected !== undefined) {
      // move the selected token
      let move = new Sim.TokenMoveOrder()
      move.x = x
      move.y = y
      move.token = this.selected
      eventBus.$emit('/client/token/move', move)

      // Avoid having the token twice in the move queue
      let midx = this.movingTokens.indexOf(this.selected)
      if (midx !== undefined) {
        this.movingTokens.splice(midx, 1)
      }
    }
  }

  clientDeleteSelectedToken () {
    if (this.hasSelection()) {
      eventBus.$emit('/client/token/delete', this.selected)
    }
  }

  clientToggleFoeSelectedToken () {
    if (this.hasSelection()) {
      eventBus.$emit('/client/token/toggle_foe', this.selected)
    }
  }

  clientSpawnTokenAt (wx: number, wy: number) {
    eventBus.$emit('/client/token/create', new Sim.Point(wx, wy))
  }

  onMouseMove (event: MouseEvent) {
    this.tool.onMouseMove(event)
  }

  onMouseUp (event: MouseEvent) {
    this.tool.onMouseUp(event)
  }

  onMouseWheel (event: WheelEvent) {
    this.renderer.camera.zoom(event.deltaY)
    this.requestRedraw()
  }

  mounted () {
    console.log('Mounted the canvas')
    // Request the inital state from the server
    eventBus.$emit('/server/request_state')

    this.canvas = this.$el.children[0] as HTMLCanvasElement
    this.canvas.width = this.$el.scrollWidth
    this.canvas.height = this.$el.scrollHeight
    this.pixelPerMeter = this.canvas.height / 10

    this.ctx = this.canvas.getContext('webgl')
    if (this.ctx) {
      this.renderer.ctx = this.ctx
      this.renderer.init()
    }

    // Handle window resizes
    window.addEventListener('resize', (ev: UIEvent) => {
      if (this.canvas) {
        this.canvas.width = this.$el.clientWidth
        this.canvas.height = this.$el.clientHeight
        this.renderer.camera.aspectRatio = this.canvas.width / this.canvas.height
        this.requestRedraw()
        this.pixelPerMeter = this.canvas.height / 10
      }
    })
    this.requestRedraw()
  }

  updateMovingTokens () {
    let delta = 0.016
    let start: number = Date.now()
    let toRemove : Sim.Token[] = []
    this.movingTokens.forEach(t => {
      let dx = t.x - t.displayX
      let dy = t.y - t.displayY
      let l = Math.hypot(dx, dy)
      if (l < 1.1 * t.displaySpeed * delta) {
        t.displayX = t.x
        t.displayY = t.y
        toRemove.push(t)
      } else {
        t.displayX += delta * t.displaySpeed * dx / l
        t.displayY += delta * t.displaySpeed * dy / l
      }
    })

    toRemove.forEach(t => {
      this.movingTokens.splice(this.movingTokens.indexOf(t), 1)
    })

    this.requestRedraw()

    if (this.movingTokens.length > 0) {
      let stop: number = Date.now()
      let timeTaken = (stop - start)
      let sleep = Math.max(0, (1000 * delta) - timeTaken)
      setTimeout(this.updateMovingTokens, sleep)
    }
  }

  onNewToken (data: Sim.Token) {
    data.displayX = data.x
    data.displayY = data.y
    this.tokens.push(data)
    this.requestRedraw()
  }

  clearTokens () {
    this.tokens.splice(0)
    this.movingTokens.splice(0)
    this.requestRedraw()
  }

  onServerState (data: ServerState) {
    // Copy the list of tokens
    this.tokens.push(...data.tokens)
    this.lines.push(...data.lines)

    this.tokens.forEach(t => {
      t.displayX = t.x
      t.displayY = t.y
    })

    if (data.building !== null) {
      this.currentBuilding = data.building
    } else {
      this.currentBuilding = undefined
    }

    this.requestRedraw()
  }

  onServerMoveToken (data : Sim.TokenMoveOrder) {
    // We use the same tokens as the server
    this.requestRedraw()
    data.token.displaySpeed = Math.hypot(data.token.x - data.token.displayX, data.token.y - data.token.displayY)
    this.movingTokens.push(data.token)
    if (this.movingTokens.length === 1) {
      this.updateMovingTokens()
    }
  }

  onServerDeleteToken (data : Sim.Token) {
    if (this.selected !== undefined && this.selected.id === data.id) {
      this.selected = undefined
    }

    let pos = this.movingTokens.indexOf(data)
    if (pos !== undefined) {
      this.movingTokens.splice(pos, 1)
    }

    pos = this.tokens.findIndex((t : Sim.Token) => { return t.id === data.id })
    if (pos >= 0) {
      this.tokens.splice(pos, 1)
      this.requestRedraw()
    } else {
      console.log('Asked to delete token', data,
        'but no token with that id is registered in the map.')
    }
  }

  onServerCreateLine (data: Sim.Line) {
    this.lines.push(data)
    this.requestRedraw()
  }

  onServerClearLines () {
    this.lines.splice(0, this.lines.length)
    this.requestRedraw()
  }

  onToolSelected (type: string) {
    if (type === 'view') {
      this.tool = new Tool(this)
    } else if (type === 'token') {
      this.tool = new ToolToken(this)
    } else if (type === 'line') {
      this.tool = new ToolLine(this)
    } else if (type === 'room') {
      this.tool = new ToolRoom(this)
    } else if (type === 'door') {
      this.tool = new ToolDoor(this)
    } else if (type === 'furniture') {
      this.tool = new ToolFurniture(this)
    }
  }

  onServerToggleFoe (token: Sim.Token) {
    this.requestRedraw()
  }

  onClientSaveBuilding () {
    if (this.currentBuilding !== undefined) {
      let data = new Blob([JSON.stringify(this.currentBuilding.toSerializable())], { type: 'application/json' })
      let fileSaver = require('../lib/filesaver/FileSaver.min.js')
      fileSaver.saveAs(data, 'Building.json')
    }
  }

  onClientLoadBuilding (file: File) {
    let reader: FileReader = new FileReader()
    reader.onload = (raw: any) => {
      console.log(reader.result)
      if (typeof reader.result === 'string') {
        let data = JSON.parse(reader.result as string)
        let b = B.Building.fromSerializable(data)
        eventBus.$emit('/client/building/set', b)
      }
    }
    reader.readAsText(file)
  }

  onServerSetBuilding (b: B.Building | null) {
    if (b === null) {
      this.currentBuilding = undefined
    } else {
      this.currentBuilding = b
    }
    this.requestRedraw()
  }
}
</script>

<style scoped>
canvas {
  outline-width: 0px !important;
}
</style>
