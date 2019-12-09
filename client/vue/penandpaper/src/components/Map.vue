<template>
  <div>
    <canvas v-on:mousedown="onMouseDown" v-on:mouseup="onMouseUp" v-on:mousemove="onMouseMove" v-on:wheel="onMouseWheel"/>
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator'
import Server from './server'
import eventBus from '../eventbus'
import { Point, Rectangle, Token } from '../simulation'

enum MouseAction {
  NONE,
  DRAG
}

@Component
export default class Map extends Vue {
  tokens: Token[] = []
  canvas?: HTMLCanvasElement

  mouseAction: MouseAction = MouseAction.NONE

  lastMouseX: number = 0
  lastMouseY: number = 0

  zoom: number = 1
  offx: number = 0
  offy: number = 0

  pixelPerMeter: number = 1

  constructor () {
    super()
    eventBus.$on('/server/token/create', (data:Token) => { this.onNewToken(data) })
  }

  renderMap () {
    if (this.canvas) {
      // Assume that without zoom 10 meters are visible vertically
      // Determine the visible area
      let visibleHeight: number = 10 / this.zoom
      let visibleWidth: number = visibleHeight * (this.canvas.width / this.canvas.height)

      let visible: Rectangle = new Rectangle()
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

      // Draw the tokens
      for (let token of this.tokens) {
        ctx.beginPath()
        ctx.arc(token.x, token.y, token.radius, 0, 2 * Math.PI)
        ctx.fillStyle = '#FF0000' // TODO: use the token color
        ctx.fill()
      }
    }
  }

  screenToWorldPos (point: Point) : Point {
    return new Point((point.x - this.$el.clientWidth / 2) / this.pixelPerMeter / this.zoom + this.offx,
      (point.y - this.$el.clientHeight / 2) / this.pixelPerMeter * this.zoom + this.offy)
  }

  worldToScreenPos (point: Point) : Point {
    return new Point((point.x - this.offx) * this.zoom * this.pixelPerMeter + this.$el.clientWidth / 2,
      (point.y - this.offy) * this.zoom * this.pixelPerMeter + this.$el.clientHeight / 2)
  }

  onMouseDown (event: MouseEvent) {
    if (event.button === 0) {
      if (event.ctrlKey) {
        let newPos = this.screenToWorldPos(new Point(event.offsetX, event.offsetY))
        eventBus.$emit('/client/token/create', newPos)
      } else {
        this.mouseAction = MouseAction.DRAG
      }
    } else if (event.button === 1) {
      this.offx = 0
      this.offy = 0
      this.zoom = 1
    }
    this.lastMouseX = event.offsetX
    this.lastMouseY = event.offsetY
  }

  onMouseMove (event: MouseEvent) {
    if (this.mouseAction === MouseAction.DRAG) {
      this.offx -= event.movementX / this.pixelPerMeter / this.zoom
      this.offy -= event.movementY / this.pixelPerMeter / this.zoom
      this.renderMap()
    }
    this.lastMouseX = event.offsetX
    this.lastMouseY = event.offsetY
  }

  onMouseUp (event: MouseEvent) {
    this.mouseAction = MouseAction.NONE
    this.lastMouseX = event.offsetX
    this.lastMouseY = event.offsetY
    this.renderMap()
  }

  onMouseWheel (event: WheelEvent) {
    if (event.deltaY > 0) {
      this.zoom *= 0.9
    } else if (event.deltaY < 0) {
      this.zoom *= 1.1
    }
    this.renderMap()
  }

  mounted () {
    console.log('Mounted the canvas')
    let map : Map = this
    this.canvas = this.$el.children[0] as HTMLCanvasElement
    this.canvas.width = this.$el.scrollWidth
    this.canvas.height = this.$el.scrollHeight
    this.pixelPerMeter = this.canvas.height / 10

    // Handle window resizes
    window.addEventListener('resize', function (ev: UIEvent) {
      if (map.canvas) {
        map.canvas.width = map.$el.clientWidth
        map.canvas.height = map.$el.clientHeight
        map.renderMap()
        map.pixelPerMeter = map.canvas.height / 10
      }
    })
    this.renderMap()
  }

  onNewToken (data: Token) {
    this.tokens.push(data)
    this.renderMap()
  }
}
</script>

<style scoped>

</style>
