/**
 * Copyright 2020 Florian Kramer
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
