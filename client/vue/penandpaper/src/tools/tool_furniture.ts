import Tool from './tool'
import Map from '../components/Map.vue'
import * as Sim from '../simulation/simulation'
import eventBus from '../eventbus'
import * as B from '../simulation/building'

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

  onMouseDown (event: MouseEvent) : boolean {
    let worldPos = this.map.screenToWorldPos(new Sim.Point(event.offsetX, event.offsetY))
    if (event.ctrlKey && event.altKey) {
      let f : B.Furniture | undefined = this.map.getFurnitureAt(worldPos)
      if (f !== undefined) {
        this.currentFurniture = B.Furniture.fromSerializable(f.toSerializable())
        this.isCloning = true
        let c = this.currentFurniture.center()
        this.cloningOffset.x = c.x - worldPos.x
        this.cloningOffset.y = c.y - worldPos.y
      }
    } else if (event.ctrlKey && this.map.getRoomAt(worldPos) !== undefined) {
      if (event.button === 2) {
        this.map.removeFurnitureAt(worldPos)
        this.map.requestRedraw()
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
          this.rotationPivot = this.rotatedFurniture.center()
        }
      }
    } else {
      return super.onMouseDown(event)
    }
    return true;
  }

  onMouseMove (event: MouseEvent) : boolean {
    let worldPos = this.map.screenToWorldPos(new Sim.Point(event.offsetX, event.offsetY))
    if (this.isDrawing) {
      this.stop.x = Math.round(worldPos.x * this.accuracy) / this.accuracy
      this.stop.y = Math.round(worldPos.y * this.accuracy) / this.accuracy
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
        this.map.addFurniture(this.currentFurniture)
      }
      this.currentFurniture = new B.Furniture()
    } else if (this.isRotating) {
      this.rotatedFurniture = undefined
    } else if (this.isCloning) {
      this.map.addFurniture(this.currentFurniture)
      this.currentFurniture = new B.Furniture()
    }
    super.onMouseUp(event)
    this.isDrawing = false
    this.isRotating = false
    this.isCloning = false
    return false
  }

  updateFurniture () {
    this.currentFurniture.min.x = Math.min(this.start.x, this.stop.x)
    this.currentFurniture.min.y = Math.min(this.start.y, this.stop.y)
    this.currentFurniture.max.x = Math.max(this.start.x, this.stop.x)
    this.currentFurniture.max.y = Math.max(this.start.y, this.stop.y)
  }

  render (ctx: CanvasRenderingContext2D) {
    if (this.isDrawing || this.isCloning) {
      // Draw the lines
      if (this.isDrawing) {
        this.updateFurniture()
      }
      ctx.lineWidth = this.map.computeLineWidth()
      this.currentFurniture.render(ctx)

      let text = this.currentFurniture.width().toFixed(1) + 'm x ' + this.currentFurniture.height().toFixed(1) + 'm'
      ctx.fillStyle = '#FFFFFF'
      this.map.setupScreenSpaceFont(ctx)
      let screenSpacePos = this.map.worldToScreenPos(new Sim.Point(this.currentFurniture.max.x, this.currentFurniture.max.y))
      let transform = ctx.getTransform()
      ctx.resetTransform()
      ctx.fillText(text, screenSpacePos.x + 10, screenSpacePos.y)
      ctx.setTransform(transform)
    }
  }
}
