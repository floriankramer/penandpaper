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

import Material from './material'
import Camera from './camera'
import ShaderCache from './shader_cache'
import Actor, { ShaderInputType } from './actor'

export default class GridMaterial extends Material {
  heightLoc: WebGLUniformLocation | null = null
  widthLoc: WebGLUniformLocation | null = null
  thresholdLoc: WebGLUniformLocation | null = null
  stepLoc: WebGLUniformLocation | null = null
  offsetLoc: WebGLUniformLocation | null = null

  constructor () {
    super()

    this.vertexShaderSrc = `
      attribute vec4 aPos;

      varying highp vec4 pos;
      void main() {
        gl_Position = aPos;
        pos = aPos;
      }
    `

    this.fragmentShaderSrc = `
      precision highp float;

      varying highp vec4 pos;

      uniform float uHeight;
      uniform float uWidth;
      uniform float uThreshold;
      uniform float uStep;

      uniform vec2 uOff;

      void main() {
        float fx = pos.x * uWidth + uOff.x;
        float fy = pos.y * uHeight + uOff.y;
        fx = fract(fx / uStep) * uStep;
        fy = fract(fy / uStep) * uStep;
        if (step(uThreshold, fx) * step(uThreshold, fy) < 1.0) {
          gl_FragColor = vec4(0.3, 0.3, 0.3, 1);
        } else {
          gl_FragColor = vec4(0.2, 0.2, 0.2, 1);
        }
      }
    `
  }

  build (ctx: WebGLRenderingContext, shaderCache: ShaderCache) {
    super.build(ctx, shaderCache)
    if (!this.program) {
      return
    }

    this.attributes.set(ShaderInputType.POSITION, ctx.getAttribLocation(this.program, 'aPos'))

    this.heightLoc = ctx.getUniformLocation(this.program, 'uHeight')
    this.widthLoc = ctx.getUniformLocation(this.program, 'uWidth')
    this.thresholdLoc = ctx.getUniformLocation(this.program, 'uThreshold')
    this.stepLoc = ctx.getUniformLocation(this.program, 'uStep')
    this.offsetLoc = ctx.getUniformLocation(this.program, 'uOff')
  }

  activate (ctx: WebGLRenderingContext, shaderCache: ShaderCache, cam: Camera, actor: Actor) {
    super.activate(ctx, shaderCache, cam, actor)
    ctx.uniform1f(this.heightLoc, cam.height)
    ctx.uniform1f(this.widthLoc, cam.height * cam.aspectRatio)

    let pixelHeight = (cam.height * 2.0) / cam.heightPixels
    ctx.uniform1f(this.thresholdLoc, pixelHeight)
    ctx.uniform1f(this.stepLoc, 5)

    ctx.uniform2f(this.offsetLoc, cam.x, cam.y)
  }
}
