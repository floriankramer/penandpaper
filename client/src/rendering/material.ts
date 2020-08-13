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

import Camera from './camera'
import ShaderCache from './shader_cache'
import Actor, { ShaderInputType } from './actor'

export default class Material {
  vertexShaderSrc: string = ''
  fragmentShaderSrc: string = ''

  program: WebGLProgram | null = null

  isBuilt: boolean = false

  attributes: Map<ShaderInputType, number> = new Map()

  build (ctx: WebGLRenderingContext, shaderCache: ShaderCache) {
    this.isBuilt = true
    let cached = shaderCache.getProgram(this.vertexShaderSrc, this.fragmentShaderSrc)
    if (cached !== undefined) {
      this.program = cached
    } else {
      let vertexShader = this.buildShader(ctx, this.vertexShaderSrc, ctx.VERTEX_SHADER)
      let fragmentShader = this.buildShader(ctx, this.fragmentShaderSrc, ctx.FRAGMENT_SHADER)

      if (vertexShader && fragmentShader) {
        this.program = ctx.createProgram()
        if (this.program) {
          ctx.attachShader(this.program, vertexShader)
          ctx.attachShader(this.program, fragmentShader)
          ctx.linkProgram(this.program)

          if (!ctx.getProgramParameter(this.program, ctx.LINK_STATUS)) {
            console.log('Error: unable to link the shader: ', ctx.getProgramInfoLog(this.program))
          }
          shaderCache.addProgram(this.vertexShaderSrc, this.fragmentShaderSrc, this.program)
        }
      }
    }
  }

  activate (ctx: WebGLRenderingContext, shaderCache: ShaderCache, cam: Camera, actor: Actor) {
    if (!this.isBuilt) {
      this.build(ctx, shaderCache)
    }
    if (this.program) {
      ctx.useProgram(this.program)
    }
  }

  buildShader (ctx: WebGLRenderingContext, src: string, type: number) : WebGLShader | null {
    let shader = ctx.createShader(type)
    if (shader) {
      ctx.shaderSource(shader, src)
      ctx.compileShader(shader)
      if (!ctx.getShaderParameter(shader, ctx.COMPILE_STATUS)) {
        console.log('Error: unable to compile the shader: ', src, ctx.getShaderInfoLog(shader))
        ctx.deleteShader(shader)
      }
    }
    return shader
  }
}
