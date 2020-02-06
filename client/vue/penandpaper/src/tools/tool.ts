import Map from '../components/Map.vue'
import * as Sim from '../simulation/simulation'

export default class Tool {
  map: Map

  isDragging: boolean = false

  constructor (map: Map) {
    this.map = map
  }

  onMouseDown (event: MouseEvent) : boolean {
    // Wether this tool did something with the given event
    let consumeEvent: boolean = false
    let worldPos = this.map.screenToWorldPos(new Sim.Point(event.offsetX, event.offsetY))
    if (event.button === 0) {
      if (this.map.selected !== undefined && event.altKey) {
        this.map.clientMoveSelectedTo(worldPos.x, worldPos.y)
        consumeEvent = true
      } else {
        let clickedToken: boolean = this.map.selectTokenAt(worldPos.x, worldPos.y)
        if (!clickedToken) {
          if (!this.map.toggleDoorAt(worldPos.x, worldPos.y)) {
            this.isDragging = true
          }
        }
        consumeEvent = true
      }
    } else if (event.button === 1) {
      this.map.resetCamera()
      consumeEvent = true
    } else if (event.button === 2) {
      this.map.clientMoveSelectedTo(worldPos.x, worldPos.y)
      consumeEvent = true
    }
    this.map.setLastMousePos(event.offsetX, event.offsetY)
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
    this.map.setLastMousePos(event.offsetX, event.offsetY)
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
    this.isDragging = false
    this.map.setLastMousePos(event.offsetX, event.offsetY)
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

  render (ctx: CanvasRenderingContext2D) {
  }
}
