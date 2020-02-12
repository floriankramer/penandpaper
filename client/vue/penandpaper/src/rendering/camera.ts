import Matrix from "./matrix"

export default class Camera {
  x: number = 0
  y: number = 0
  height: number = 10.01
  aspectRatio: number = 16 / 9

  heightPixels: number = 1

  // A 3x3 matrix
  matrix: Matrix = new Matrix()

  getMatrix() : Float32Array {
    return this.matrix.data
  }

  updateMatrix() {
    this.matrix.setIdentity()
    this.matrix.translate(-this.x, -this.y)
    this.matrix.scale(1 / this.height / this.aspectRatio, 1 / this.height)
  }

  zoom (dz: number) {
    if (dz < 0) {
      this.height *= 0.85
    } else if (dz > 0) {
      this.height *= 1.15
    }
    this.updateMatrix()
  }

  pan (dx: number, dy: number) {
    this.x -= dx * this.height * this.aspectRatio
    this.y -= dy * this.height
    this.updateMatrix()
  }

  reset () {
    this.height = 10.01
    this.x = 0
    this.y = 0
    this.updateMatrix()
  }
}