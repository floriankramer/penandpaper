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

export default class ToolToken extends Tool {
  onMouseDown (event: MouseEvent) : boolean {
    if (event.ctrlKey) {
      let worldPos = this.map.screenToWorldPos(new Sim.Point(event.offsetX, event.offsetY))
      this.map.clientSpawnTokenAt(worldPos.x, worldPos.y)
      return true
    } else {
      return super.onMouseDown(event)
    }
  }
}
