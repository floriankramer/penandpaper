
export class Room {
  minX: number = 0
  maxX: number = 0
  minY: number = 0
  maxY: number = 0

  width () : number {
    return this.maxX - this.minX
  }

  height () : number {
    return this.maxY - this.minY
  }

  render (ctx: CanvasRenderingContext2D) {
    ctx.fillStyle = '#444444'
    ctx.fillRect(this.minX, this.minY, this.width(), this.height())

    ctx.strokeStyle = '#FFFFFF'
    ctx.beginPath()
    ctx.moveTo(this.minX, this.minY)
    ctx.lineTo(this.maxX, this.minY)
    ctx.lineTo(this.maxX, this.maxY)
    ctx.lineTo(this.minX, this.maxY)
    ctx.lineTo(this.minX, this.minY)
    ctx.stroke()
  }
}

export class Building {
  rooms: Room[] = []

  render (ctx: CanvasRenderingContext2D) {
    this.rooms.forEach(room => {
      room.render(ctx)
    })
  }
}
