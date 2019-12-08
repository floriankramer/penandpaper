<template>
  <canvas />
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator'
import Server from './server'
import eventBus from '../eventbus'

class Token {
  x: number = 0;
  y: number = 0;
  radius: number = 0;
}

class Rectangle {
  minx : number = 0;
  maxx : number = 0;
  miny : number = 0;
  maxy : number = 0;
}

@Component
export default class Map extends Vue {
  tokens: Token[] = []
  canvas?: HTMLCanvasElement

  zoom: number = 1
  offx: number = 0
  offy: number = 0

  renderMap () {
    if (this.canvas) {
      // Assume that without zoom 10 meters are visible vertically
      let pixelPerMeter: number = 10 / this.canvas.height

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
        return
      }

      ctx.fillStyle = '#333333'
      ctx.fillRect(0, 0, this.canvas.width, this.canvas.height)

      // TODO: draw the grid
    }
  }

  mounted () {
    this.canvas = this.$el as HTMLCanvasElement
    this.renderMap()
  }
}
</script>

<style scoped>

</style>
