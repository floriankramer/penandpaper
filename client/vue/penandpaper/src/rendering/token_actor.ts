import Actor from "./actor";
import DiffuseMaterial from "./diffuse_material"

export default class TokenActor extends Actor {
  constructor() {
    super()
    this.material = new DiffuseMaterial()
    // A circle
    let sampleCount = 32
    let step = 2 * Math.PI / sampleCount
    for (let i = 0; i < sampleCount; ++i) {
      let a = i * step
      let a2 = (i + 1) * step
      this.positions.push(0, 0)
      this.positions.push(Math.cos(a), Math.sin(a))
      this.positions.push(Math.cos(a2), Math.sin(a2))
    }
  }

  setColor (r: number, g: number, b: number) {
    let dm = this.material as DiffuseMaterial
    dm.r = r
    dm.g = g
    dm.b = b
  }
}