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

import Tool from './tool'
import Map from '../components/Map.vue'
import * as Sim from '../simulation/simulation'
import eventBus from '../eventbus'
import * as B from '../simulation/building'

export default class ToolReveal extends Tool {

  onMouseDown (event: MouseEvent) : boolean {
    if (event.altKey) {
      let worldPos = this.map.screenToWorldPos(new Sim.Point(event.offsetX, event.offsetY))
      this.map.revealWallAt(worldPos.x, worldPos.y)
      return true
    } else {
      return super.onMouseDown(event)
    }
  }


  render (ctx: CanvasRenderingContext2D) {
  }
}
