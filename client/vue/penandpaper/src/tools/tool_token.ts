import Tool from './tool'
import Map from '../components/Map.vue'
import * as Sim from '../simulation'

export default class ToolToken extends Tool {
  onMouseDown (event: MouseEvent) : boolean {
    if (event.ctrlKey) {
      let worldPos = this.map.screenToWorldPos(new Sim.Point(event.offsetX, event.offsetY))
      this.map.clientSpawnTokenAt(worldPos.x, worldPos.y)
      return true
    } else {
      return super.onMouseDown(event)
    }
  }
}
