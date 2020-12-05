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
import * as Sim from '../simulation/simulation'
import eventBus from '../eventbus'

export default class ToolDistance extends Tool {
  lastPing = Date.now() - 1000

  onMouseDown (event: MouseEvent) : boolean {
    if (event.button === 0) {
      let now = Date.now()
      if (now - this.lastPing > 1000) {
        let worldPos = this.map.screenToWorldPos(new Sim.Point(event.offsetX, event.offsetY))
        eventBus.$emit('/client/ping-at', worldPos.x, worldPos.y)
        this.lastPing = now
      }
    }
    return true
  }

  onMouseMove (event: MouseEvent) : boolean {
    return false
  }

  onMouseUp (event: MouseEvent) : boolean {
    return false
  }
}
