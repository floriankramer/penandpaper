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
  constructor () {
    super()
    this.material = new FontMaterial()
    // A full screen rect
    let positions = [-15, -15, -15, 15, 15, -15, -15, 15, 15, -15, 15, 15]
    let uv = [0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0]
    this.vertexShaderInput.set(ShaderInputType.POSITION, positions)
    this.vertexShaderInput.set(ShaderInputType.TEXTURE_COORD, uv)
  }

  setText(text: string) {
    let font: F.Font = F.FontLoader.default
    let offX = 0
    let positions: number[] = []
    let uv: number[] = []
    for (let i = 0; i < text.length; ++i) {
      if (text[i] == ' ') {
        // TODO: this is a hack
        offX += 1
        continue
      }
      let g = font.glyphs.get(text[i])
      if (g === undefined) {
        g = font.glyphs.get('a')
        if (g === undefined) {
          continue
        }
      }
      console.log(g)
      let sx = offX + g.bearingX
      let offY = g.bearingY - g.height
      positions.push(
        // first triangle
        sx, offY + g.height,
        sx, offY,
        sx + g.width, offY + g.height,
        // second triangle
        sx + g.width, offY + g.height,
        sx, offY,
        sx + g.width, offY
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
      offX += g.advance
    }
    this.vertexShaderInput.set(ShaderInputType.POSITION, positions)
    this.vertexShaderInput.set(ShaderInputType.TEXTURE_COORD, uv)
    this.setVertexDataChanged()
  }
}
