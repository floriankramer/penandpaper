import Actor, { ShaderInputType } from "./actor";
import GridMaterial from "./grid_material"

export default class GridActor extends Actor {
  constructor() {
    super()
    this.material = new GridMaterial()
    // A full screen rect
    let positions = [-1, -1, -1, 1, 1, -1, -1, 1, 1, -1, 1, 1]
    this.vertexShaderInput.set(ShaderInputType.POSITION, positions)
  }
}