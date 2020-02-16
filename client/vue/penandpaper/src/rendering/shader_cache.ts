

class ShaderEntry {
  program: WebGLProgram
  count: number = 0

  constructor (p: WebGLProgram, c: number) {
    this.program = p
    this.count = c
  }
}

export default class ShaderCache {
  programs: Map<string, ShaderEntry> = new Map()

  getProgram (vertexSrc: string, fragmentSrc: string) : WebGLProgram | undefined {
    let s = vertexSrc + fragmentSrc
    let e = this.programs.get(s)
    if (e) {
      e.count += 1
      return e.program
    } else {
      return undefined
    }
  }

  addProgram (vertexSrc: string, fragmentSrc: string, program: WebGLProgram) {
    let s = vertexSrc + fragmentSrc
    this.programs.set(s, new ShaderEntry(program, 1))
  }
}