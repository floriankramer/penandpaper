export class Rectangle {
  minx : number = 0;
  maxx : number = 0;
  miny : number = 0;
  maxy : number = 0;
}

export class Point {
  x: number = 0;
  y: number = 0;

  constructor (x: number, y: number) {
    this.x = x
    this.y = y
  }
}

export class Token {
  x: number = 0;
  y: number = 0;
  radius: number = 0.25;
  id: number = -1;
}
