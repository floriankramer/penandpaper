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
import * as Sim from '../simulation'

enum MouseAction {
  NONE,
  DRAG,
  CREATE_LINE
}

enum ToolType {
  TOKEN,
  LINE
}

@Component
export default class Map extends Vue {
  tokens: Sim.Token[] = []
  lines: Sim.Line[] = []
  canvas?: HTMLCanvasElement

  mouseAction: MouseAction = MouseAction.NONE
  toolType: ToolType = ToolType.TOKEN

  lastMouseX: number = 0
  lastMouseY: number = 0

  zoom: number = 1
  offx: number = 0
  offy: number = 0

  pixelPerMeter: number = 1

  selected?: Sim.Token

  renderQueued: boolean = false
  lastRender: number = 0

  lastLineStop: Sim.Point = new Sim.Point(0, 0)

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

    eventBus.$on('/tools/tool_token', () => { this.onToolTokenSelected() })
    eventBus.$on('/tools/tool_line', () => { this.onToolLineSelected() })
  }

  renderMap () {
    // Limit the render framerate to once every 16 ms (60 fps)
    let now = new Date().getTime()
    let sinceLast = now - this.lastRender
    if (sinceLast < 16) {
      setTimeout(this.renderMap, 16 - sinceLast)
      return
    }
    this.lastRender = now
    this.renderQueued = false

    if (this.canvas) {
      // Assume that without zoom 10 meters are visible vertically
      // Determine the visible area
      let visibleHeight: number = 10 / this.zoom
      let visibleWidth: number = visibleHeight * (this.canvas.width / this.canvas.height)

      let visible: Sim.Rectangle = new Sim.Rectangle()
      visible.minx = this.offx - visibleWidth / 2
      visible.maxx = this.offx + visibleWidth / 2
      visible.miny = this.offy - visibleHeight / 2
      visible.maxy = this.offy + visibleHeight / 2

      // Clear the canvas
      let ctx: CanvasRenderingContext2D | null = this.canvas.getContext('2d')
      if (ctx === null) {
        console.log('The canvas does not have a drawing context.')
        return
      }
      ctx.resetTransform()

      ctx.fillStyle = '#333333'
      ctx.fillRect(0, 0, this.canvas.width, this.canvas.height)

      // Make 0,0 the center
      ctx.translate(this.canvas.width / 2, this.canvas.height / 2)
      // Apply the base scale
      ctx.scale(this.pixelPerMeter, this.pixelPerMeter)

      // apply the offset
      ctx.scale(this.zoom, this.zoom)
      ctx.translate(-this.offx, -this.offy)

      // Draw a 5x5 m grid centered on 0, 0 (0, 0 is a crossign point of two grid lines)
      let gridXMin = Math.floor(visible.minx / 5) * 5
      let gridYMin = Math.floor(visible.miny / 5) * 5
      let gridXMax = Math.ceil(visible.maxx / 5) * 5
      let gridYMax = Math.ceil(visible.maxy / 5) * 5

      ctx.beginPath()
      for (let x : number = gridXMin; x <= gridXMax; x += 5) {
        ctx.moveTo(x, visible.miny)
        ctx.lineTo(x, visible.maxy)
      }
      for (let y : number = gridYMin; y <= gridYMax; y += 5) {
        ctx.moveTo(visible.minx, y)
        ctx.lineTo(visible.maxx, y)
      }
      ctx.lineWidth = 1 / this.pixelPerMeter / this.zoom
      ctx.strokeStyle = '#4c4c4c'
      ctx.stroke()

      // Draw the lines
      ctx.strokeStyle = '#FFFFEE'
      ctx.lineWidth = 2 / this.pixelPerMeter / this.zoom
      ctx.beginPath()
      for (let line of this.lines) {
        ctx.moveTo(line.start.x, line.start.y)
        ctx.lineTo(line.stop.x, line.stop.y)
      }
      ctx.stroke()

      // Draw the tokens
      for (let token of this.tokens) {
        ctx.beginPath()
        ctx.arc(token.x, token.y, token.radius, 0, 2 * Math.PI)
        ctx.fillStyle = token.color.toHex()
        ctx.fill()

        if (this.selected !== undefined && this.selected.id === token.id) {
          if (token.isFoe) {
            ctx.strokeStyle = '#FF0000'
            ctx.lineWidth = 0.04
            ctx.stroke()
          } else {
            ctx.strokeStyle = '#FFFFFF'
            ctx.lineWidth = 0.03
            ctx.stroke()
          }
        } else if (token.isFoe) {
          ctx.strokeStyle = '#FF0000'
          ctx.lineWidth = 0.05
          ctx.stroke()
        }
      }

      if (this.selected !== undefined) {
        // Draw a line from the selected to the cursor
        let worldPos = this.screenToWorldPos(new Sim.Point(this.lastMouseX, this.lastMouseY))
        ctx.strokeStyle = '#696969'
        ctx.lineWidth = 1.5 / this.pixelPerMeter / this.zoom

        ctx.beginPath()
        ctx.moveTo(this.selected.x, this.selected.y)
        ctx.lineTo(worldPos.x, worldPos.y)
        ctx.stroke()

        // Draw a distance at the cursor
        let dist = Math.hypot(worldPos.x - this.selected.x, worldPos.y - this.selected.y)
        ctx.fillStyle = ctx.strokeStyle
        ctx.font = '30px sans-serif'
        ctx.fillStyle = '#AAAAAA'
        let transform = ctx.getTransform()
        ctx.resetTransform()
        ctx.fillText(dist.toFixed(2).toString() + 'm', this.lastMouseX + 20, this.lastMouseY - 20)
        ctx.setTransform(transform)
      }
    }
  }

  screenToWorldPos (point: Sim.Point) : Sim.Point {
    return new Sim.Point((point.x - this.$el.clientWidth / 2) / this.pixelPerMeter / this.zoom + this.offx,
      (point.y - this.$el.clientHeight / 2) / this.pixelPerMeter / this.zoom + this.offy)
  }

  worldToScreenPos (point: Sim.Point) : Sim.Point {
    return new Sim.Point((point.x - this.offx) * this.zoom * this.pixelPerMeter + this.$el.clientWidth / 2,
      (point.y - this.offy) * this.zoom * this.pixelPerMeter + this.$el.clientHeight / 2)
  }

  onKeyDown (event: KeyboardEvent) {
    console.log(event)
    if (event.key === 'Delete') {
      if (this.selected !== undefined) {
        eventBus.$emit('/client/token/delete', this.selected)
      }
    } else if (event.key === 'f') {
      if (this.selected !== undefined) {
        eventBus.$emit('/client/token/toggle_foe', this.selected)
      }
    }
  }

  onMouseDown (event: MouseEvent) {
    console.log(this)
    let worldPos = this.screenToWorldPos(new Sim.Point(event.offsetX, event.offsetY))
    if (event.button === 0) {
      if (event.ctrlKey) {
        if (this.toolType === ToolType.TOKEN) {
          eventBus.$emit('/client/token/create', worldPos)
        } else if (this.toolType === ToolType.LINE) {
          this.mouseAction = MouseAction.CREATE_LINE
          this.lastLineStop.x = worldPos.x
          this.lastLineStop.y = worldPos.y
        }
      } else {
        let clickedToken: boolean = false
        for (let token of this.tokens) {
          if (Math.hypot(token.x - worldPos.x, token.y - worldPos.y) < token.radius) {
            this.selected = token
            clickedToken = true
            break
          }
        }
        if (!clickedToken) {
          this.selected = undefined
          this.mouseAction = MouseAction.DRAG
        }
      }
    } else if (event.button === 1) {
      this.offx = 0
      this.offy = 0
      this.zoom = 1
    } else if (event.button === 2) {
      if (this.selected !== undefined) {
        // move the selected token
        let move = new Sim.TokenMoveOrder()
        move.x = worldPos.x
        move.y = worldPos.y
        move.token = this.selected
        eventBus.$emit('/client/token/move', move)
      }
    }
    this.lastMouseX = event.offsetX
    this.lastMouseY = event.offsetY
  }

  onMouseMove (event: MouseEvent) {
    let doRender = false
    if (this.mouseAction === MouseAction.DRAG) {
      this.offx -= event.movementX / this.pixelPerMeter / this.zoom
      this.offy -= event.movementY / this.pixelPerMeter / this.zoom
      doRender = true
    } else if (this.mouseAction === MouseAction.CREATE_LINE) {
      let lastLineScreen = this.worldToScreenPos(this.lastLineStop)
      if (Math.hypot(lastLineScreen.x - event.offsetX, lastLineScreen.y - event.offsetY) > this.$el.clientHeight / 20) {
        // create a new line
        let worldPos = this.screenToWorldPos(new Sim.Point(event.offsetX, event.offsetY))
        let line = new Sim.Line()
        line.start.x = this.lastLineStop.x
        line.start.y = this.lastLineStop.y
        line.stop.x = worldPos.x
        line.stop.y = worldPos.y
        eventBus.$emit('/client/line/create', line)
        this.lastLineStop = worldPos
        doRender = true
      }
    }
    this.lastMouseX = event.offsetX
    this.lastMouseY = event.offsetY
    if (this.selected !== undefined) {
      // Redraw for the distance measurement
      doRender = true
    }
    if (doRender) {
      this.renderMap()
    }
  }

  onMouseUp (event: MouseEvent) {
    if (this.mouseAction === MouseAction.CREATE_LINE) {
      // create a new line
      let worldPos = this.screenToWorldPos(new Sim.Point(event.offsetX, event.offsetY))
      let line = new Sim.Line()
      line.start.x = this.lastLineStop.x
      line.start.y = this.lastLineStop.y
      line.stop.x = worldPos.x
      line.stop.y = worldPos.y
      eventBus.$emit('/client/line/create', line)
      this.lastLineStop = worldPos
    }
    this.mouseAction = MouseAction.NONE
    this.lastMouseX = event.offsetX
    this.lastMouseY = event.offsetY
    this.renderMap()
  }

  onMouseWheel (event: WheelEvent) {
    if (event.deltaY > 0) {
      this.zoom *= 0.85
    } else if (event.deltaY < 0) {
      this.zoom *= 1.15
    }
    this.renderMap()
  }

  mounted () {
    console.log('Mounted the canvas')
    // Request the inital state from the server
    eventBus.$emit('/server/request_state')

    this.canvas = this.$el.children[0] as HTMLCanvasElement
    this.canvas.width = this.$el.scrollWidth
    this.canvas.height = this.$el.scrollHeight
    this.pixelPerMeter = this.canvas.height / 10

    // Handle window resizes
    window.addEventListener('resize', (ev: UIEvent) => {
      if (this.canvas) {
        this.canvas.width = this.$el.clientWidth
        this.canvas.height = this.$el.clientHeight
        this.renderMap()
        this.pixelPerMeter = this.canvas.height / 10
      }
    })
    this.renderMap()
  }

  onNewToken (data: Sim.Token) {
    this.tokens.push(data)
    this.renderMap()
  }

  clearTokens () {
    this.tokens.splice(0)
    this.renderMap()
  }

  onServerState (data: ServerState) {
    // Copy the list of tokens
    this.tokens.push(...data.tokens)
    this.lines.push(...data.lines)
    this.renderMap()
  }

  onServerMoveToken (data : Sim.TokenMoveOrder) {
    // We use the same tokens as the server
    this.renderMap()
  }

  onServerDeleteToken (data : Sim.Token) {
    if (this.selected !== undefined && this.selected.id === data.id) {
      this.selected = undefined
    }
    let pos = this.tokens.findIndex((t : Sim.Token) => { return t.id === data.id })
    if (pos > 0) {
      this.tokens.splice(pos, 1)
      this.renderMap()
    } else {
      console.log('Asked to delete token', data,
        'but no token with that id is registered in the map.')
    }
  }

  onServerCreateLine (data: Sim.Line) {
    this.lines.push(data)
    this.renderMap()
  }

  onServerClearLines () {
    this.lines.splice(0, this.lines.length)
    this.renderMap()
  }

  onToolLineSelected () {
    this.toolType = ToolType.LINE
  }

  onToolTokenSelected () {
    this.toolType = ToolType.TOKEN
  }

  onServerToggleFoe (token: Sim.Token) {
    this.renderMap()
  }
}
</script>

<style scoped>
canvas {
  outline-width: 0px !important;
}
</style>
