

export default class ShaderCache {
  programs = {}

  getProgram (vertexSrc: string, fragmentSrc: string) : WebGLProgram | undefined {
    let s = vertexSrc + fragmentSrc
    if (s in this.programs) {
      this.programs[s].count += 1
      return this.programs[s].program
    } else {
      return undefined
    }
  }

  addProgram (vertexSrc: string, fragmentSrc: string, program: WebGLProgram) {
    let s = vertexSrc + fragmentSrc
    this.programs[s] = {
      'count': 1,
      'program': program
    }
  }
}