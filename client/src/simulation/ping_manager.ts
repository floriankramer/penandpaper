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

import PingActor from '../rendering/ping_actor'
import Renderer from '../rendering/renderer'
import RenderLayers from '../components/renderlayers'

class Ping {
  x: number = 0
  y: number = 0
  age: number = 0
  actor: PingActor = new PingActor()
}

/**
 * A class for managing pings. This updates them and adds and removes actors
 * as required
 */
export default class PingManager {
  _renderer: Renderer | null = null
  _requestRedraw: () => void = () => {}

  pings: Ping[] = []

  setRenderer (renderer: Renderer) {
    this._renderer = renderer
  }

  setRequestRedraw (requestRedraw: () => void) {
    this._requestRedraw = requestRedraw
  }

  addPing (x: number, y: number) {
    console.log('ping ', x, y)
    let p = new Ping()
    p.x = x
    p.y = y
    p.actor.setPosition(x, y)
    p.actor.setScale(0, 0)
    if (this._renderer !== null) {
      this._renderer.addActor(p.actor, RenderLayers.TOOL)
    } else {
      console.log('PingManager::addPing: A ping was added but no renderer is available.')
    }
    this.pings.push(p)
    if (this.pings.length === 1) {
      setTimeout(() => { this._update() }, 16)
    }
  }

  _update () {
    let delta = 0.016
    let start: number = Date.now()

    // Update all the pings
    for (let i = 0; i < this.pings.length; ++i) {
      let ping = this.pings[i]
      ping.age += delta
      // Use a parabola for the ping size
      let f = ping.age - 1
      let size = 1 - (f * f)
      ping.actor.setScale(size, size)
      if (ping.age > 2) {
        if (this._renderer !== null) {
          this._renderer.removeActor(ping.actor)
        }
        this.pings.splice(i, 1)
        i--
      }
    }

    this._requestRedraw()

    if (this.pings.length > 0) {
      let stop: number = Date.now()
      let timeTaken = (stop - start)
      let sleep = Math.max(0, (1000 * delta) - timeTaken)
      setTimeout(() => { this._update() }, sleep)
    }
  }
}
