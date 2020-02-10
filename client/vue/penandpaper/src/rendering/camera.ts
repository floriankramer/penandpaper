
export default class Camera {
  x: number = 0
  y: number = 0
  height: number = 1
  aspectRatio: number = 16 / 9

  // A 3x3 matrix
  cache: Float32Array = new Float32Array(9)

  getMatrix() : Float32Array {
    return this.cache
  }

  updateMatrix() {
    // row 1
    this.cache[0] = 2 / this.height
    this.cache[1] = 0
    this.cache[2] = 0

    // row 2
    this.cache[3] = 0
    this.cache[4] = 2 / this.height
    this.cache[5] = 0

    // row 3
    this.cache[6] = this.x
    this.cache[7] = this.y
    this.cache[8] = 2 / this.height
  }

  zoom (dz: number) {
    if (dz > 0) {
      this.height *= 0.85
    } else if (dz < 0) {
      this.height *= 1.15
    }
    this.updateMatrix()
  }

  pan (dx: number, dy: number) {
    this.x -= dx / this.height
    this.y -= dy / this.height
    this.updateMatrix()
  }
}