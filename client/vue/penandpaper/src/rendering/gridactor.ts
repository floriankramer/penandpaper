import Actor from "./actor";
import GridMaterial from "./grid_material"

export default class GridActor extends Actor {
  constructor() {
    super()
    this.material = new GridMaterial()
    // A full screen rect
    this.positions = [-1, -1, -1, 1, 1, -1, -1, 1, 1, -1, 1, 1]
  }
}