import Tool from './tool'
import Map from '../components/Map.vue'
import * as Sim from '../simulation/simulation'
import eventBus from '../eventbus'
import * as B from '../simulation/building'

export default class ToolRoom extends Tool {
  isDrawing: boolean = false
  start: Sim.Point = new Sim.Point(0, 0)
  stop: Sim.Point = new Sim.Point(0, 0)

  accuracy: number = 4

  currentRoom: B.Room = new B.Room()

  onMouseDown (event: MouseEvent) : boolean {
    if (event.ctrlKey) {
      let worldPos = this.map.screenToWorldPos(new Sim.Point(event.offsetX, event.offsetY))
      if (event.button === 2) {
        this.map.removeRoomAt(worldPos.x, worldPos.y)
        this.map.requestRedraw()
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
      this.map.requestRedraw()
    } else {
      super.onMouseMove(event)
      this.start.x = Math.round(worldPos.x * this.accuracy) / this.accuracy
      this.start.y = Math.round(worldPos.y * this.accuracy) / this.accuracy
      this.stop.x = this.start.x + 0.4
      this.stop.y = this.start.y + 0.4
      this.updateRoom()
      this.map.requestRedraw()
    }
    return false
  }

  onMouseUp (event: MouseEvent) : boolean {
    if (this.isDrawing) {
      // add the room to the map
      this.updateRoom()
      if (this.currentRoom.width() > 0.1 && this.currentRoom.height() > 0.1) {
        this.map.addRoom(this.currentRoom)
      }
      this.currentRoom = new B.Room()
    }
    super.onMouseUp(event)
    this.isDrawing = false
    return false
  }

  updateRoom () {
    this.currentRoom.min.x = Math.min(this.start.x, this.stop.x)
    this.currentRoom.min.y = Math.min(this.start.y, this.stop.y)
    this.currentRoom.max.x = Math.max(this.start.x, this.stop.x)
    this.currentRoom.max.y = Math.max(this.start.y, this.stop.y)
  }

  render (ctx: CanvasRenderingContext2D) {
    if (this.isDrawing) {
      // Draw the lines
      this.updateRoom()
      ctx.lineWidth = this.map.computeLineWidth()
      this.currentRoom.render(ctx)

      let text = this.currentRoom.width().toFixed(1) + 'm x ' + this.currentRoom.height().toFixed(1) + 'm'
      ctx.fillStyle = '#FFFFFF'
      this.map.setupScreenSpaceFont(ctx)
      let screenSpacePos = this.map.worldToScreenPos(new Sim.Point(this.currentRoom.max.x, this.currentRoom.max.y))
      let transform = ctx.getTransform()
      ctx.resetTransform()
      ctx.fillText(text, screenSpacePos.x + 10, screenSpacePos.y)
      ctx.setTransform(transform)
    } else {
      ctx.lineWidth = this.map.computeLineWidth()
      this.currentRoom.render(ctx)
    }
  }
}
