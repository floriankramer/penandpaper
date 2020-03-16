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

import Actor, { ShaderInputType } from './actor'
import FontMaterial from './fontmaterial'
import * as F from './fontloader'

export default class GridActor extends Actor {
  text: string = ''
  textDirty = false

  constructor () {
    super()
    this.material = new FontMaterial()
    // A full screen rect
    let positions = [-15, -15, -15, 15, 15, -15, -15, 15, 15, -15, 15, 15]
    let uv = [0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0]
    this.vertexShaderInput.set(ShaderInputType.POSITION, positions)
    this.vertexShaderInput.set(ShaderInputType.TEXTURE_COORD, uv)
  }

  setText (text: string) {
    this.text = text
    this.textDirty = true
  }

  activate (ctx: WebGLRenderingContext) {
    if (this.textDirty && F.FontLoader.default.ready) {
      this.rebuildMesh()
    }
    super.activate(ctx)
  }

  rebuildMesh () {
    let font: F.Font = F.FontLoader.default
    let s = font.scale
    let offX = 0
    let positions: number[] = []
    let uv: number[] = []
    for (let i = 0; i < this.text.length; ++i) {
      let g = font.glyphs.get(this.text[i])
      if (g === undefined) {
        g = font.glyphs.get('a')
        if (g === undefined) {
          continue
        }
      }
      if (g.character !== ' ') {
        let sx = offX + g.bearingX * s
        let offY = (g.bearingY - g.height) * s
        positions.push(
          // first triangle
          sx, offY + g.height * s,
          sx, offY,
          sx + g.width * s, offY + g.height * s,
          // second triangle
          sx + g.width * s, offY + g.height * s,
          sx, offY,
          sx + g.width * s, offY
        )
        uv.push(
          // first triangle
          g.uMin, g.vMin,
          g.uMin, g.vMax,
          g.uMax, g.vMin,
          // second triangle
          g.uMax, g.vMin,
          g.uMin, g.vMax,
          g.uMax, g.vMax
        )
      }
      offX += g.advance * s
    }
    this.vertexShaderInput.set(ShaderInputType.POSITION, positions)
    this.vertexShaderInput.set(ShaderInputType.TEXTURE_COORD, uv)
    this.setVertexDataChanged()
    this.textDirty = false
  }
}
