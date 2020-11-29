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

import Renderer from './renderer'
import TileActor from './tileactor'
import $ from 'jquery'
import RenderLayers from '../components/renderlayers'

export default class TileRenderer {
  renderer: Renderer | null = null
  actors: TileActor[] = []

  width: number = 0
  height: number = 0
  tilesize: number = 1

  renderCallback: null | (() => any) = null

  // TODO as a test load and render the tiles from tiles/test2/0/{-2..2}/{-2..2}.png

  init (r: Renderer) {
    this.renderer = r
  }

  setRenderCallback (callback: (() => any) | null) {
    this.renderCallback = callback
  }

  loadFrom (path: string) {
    this.clear()
    let jsonPath = path + '/meta.json'

    $.get(jsonPath, (meta) => {
      this.width = meta['width']
      this.height = meta['height']
      this.tilesize = meta['tilesize']

      for (let y = 0; y < this.height; ++y) {
        for (let x = 0; x < this.width; ++x) {
          let a = new TileActor()
          a.setRenderCallback(this.renderCallback)
          a.setTexturePath(path + '/0/' + y.toFixed(0).toString() + '/' + x.toFixed(0).toString() + '.png')
          this.actors.push(a)
          a.setScale((this.tilesize + 0.001) / 2, -(this.tilesize + 0.001) / 2)
          a.setPosition((x - this.width / 2.0) * this.tilesize, (this.height / 2.0 - y) * this.tilesize)
          if (this.renderer !== null) {
            this.renderer.addActor(a, RenderLayers.TILES)
          } else {
            console.log('Unable to add tile actor, the tilerenderer was not initialized.')
          }
        }
      }
    })
  }

  clear () {
    this.actors.forEach((actor) => {
      if (this.renderer !== null) {
        this.renderer.removeActor(actor)
      }
    })
    this.actors.splice(0, this.actors.length)
    if (this.renderCallback !== null) {
      this.renderCallback()
    }
  }
}
