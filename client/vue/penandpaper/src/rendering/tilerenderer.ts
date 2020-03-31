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

      console.log(meta)
      for (let y = 0; y < this.width; ++y) {
        for (let x = 0; x < this.height; ++x) {
          console.log(x, y)
          let a = new TileActor()
          a.setRenderCallback(this.renderCallback)
          a.setTexturePath(path + '/0/' + y.toFixed(0).toString() + '/' + x.toFixed(0).toString() + '.png')
          this.actors.push(a)
          a.setScale((this.tilesize + 0.001) / 2, (this.tilesize + 0.001) / 2)
          a.setPosition((x - this.width / 2.0) * this.tilesize, (y - this.height / 2.0) * this.tilesize)
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
