<!--
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
-->

<template>
  <div>
    <div class="toolbar-container">
      <div class="toolbar-group" v-for="toolGroup in toolGroups" v-bind:key="toolGroup">
        <label v-for="tool in toolGroup" v-bind:key="tool">
          <input type="radio" name="current_tool" v-bind:value="tool.tool"
            v-model='currentTool' v-on:change='selectTool(tool.tool)'
            v-bind:title="tool.tooltip">
          <img v-bind:src="tool.icon" width="25" height="25">
        </label>
      </div>
      <div class="toolbar-group" v-if="showGmIcons">
        <label>
          <input type="button" v-on:click="clearTokens">
          <img src="images/clear_tokens.svg" width="25" height="25">
        </label>
        <label>
          <input type="button" v-on:click="clearLines">
          <img src="images/clear_lines.svg" width="25" height="25">
        </label>
        <label>
          <input type="button" v-on:click="clearBuilding">
          <img src="images/clear_building.svg" width="25" height="25">
        </label>
      </div>
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator'
import Server from './server'
import eventBus from '../eventbus'

class ToolButton {
  icon: string = ''
  tool: string = ''
  tooltip: string = ''
}

@Component
export default class Toolbar extends Vue {
  currentTool: string = 'view'

  showGmIcons: boolean = false

  toolGroupsAll: ToolButton[][] = [
    [
      {
        'icon': 'images/tool_view.svg',
        'tool': 'view',
        'tooltip': 'Move the camera and tokens.'
      }
    ]
  ]

  toolGroupsGm: ToolButton[][] = [
    [
      {
        'icon': 'images/tool_token.svg',
        'tool': 'token',
        'tooltip': 'Create new tokens.'
      },
      {
        'icon': 'images/tool_line.svg',
        'tool': 'line',
        'tooltip': 'Create new lines.'
      }
    ],
    [
      {
        'icon': 'images/tool_room.svg',
        'tool': 'room',
        'tooltip': 'Create new room.'
      },
      {
        'icon': 'images/tool_wall.svg',
        'tool': 'wall',
        'tooltip': 'Create new walls.'
      },
      {
        'icon': 'images/tool_door.svg',
        'tool': 'door',
        'tooltip': 'Create new doors.'
      },
      {
        'icon': 'images/tool_furniture.svg',
        'tool': 'furniture',
        'tooltip': 'Create new furniture.'
      },
      {
        'icon': 'images/tool_reveal.svg',
        'tool': 'reveal',
        'tooltip': 'Reveal and hide building parts.'
      }
    ]
  ]

  toolGroups: ToolButton[][] = []

  selectTool (tool: string) {
    eventBus.$emit('/tools/select_tool', tool)
  }

  clearTokens () {
    eventBus.$emit('/client/token/clear')
  }

  clearLines () {
    eventBus.$emit('/client/line/clear')
  }

  clearBuilding () {
    eventBus.$emit('/client/building/clear')
  }

  mounted () {
    this.updateToolGroups()
    eventBus.$on('/client/is-gamemaster', (isGm: boolean) => {
      this.showGmIcons = isGm
      this.updateToolGroups()
    })
  }

  updateToolGroups () {
    this.toolGroups = this.toolGroupsAll.slice()
    if (this.showGmIcons) {
      this.toolGroups = this.toolGroups.concat(this.toolGroupsGm)
    }
  }
}
</script>

<style scoped>
div .toolbar-container {
  height: calc(100% - 5px);
  width: calc(100% - 20px);
  padding-left: 10px;
  padding-right: 10px;
  padding-top: 5px;
}

div .toolbar-group {
  float: left;
  border-right: 2px solid #756e63;
  margin-right: 10px;
}

.toolbar-align-right {
  float: right;
}

.toolbar-center-verticaly {
  margin-top: 4px;
}

.toolbar-margin-right {
  margin-right: 14px;
}

/** Image radio buttons: https://stackoverflow.com/questions/17541614/use-images-instead-of-radio-buttons*/
/* HIDE RADIO */
[type=radio] {
  position: absolute;
  opacity: 0;
  width: 0;
  height: 0;
  left: -100%;
}

/* IMAGE STYLES */
[type=radio] + img {
  cursor: pointer;
  margin-right: 10px;
}

/* CHECKED STYLES */
[type=radio]:checked + img {
  border: 1px solid rgb(200, 200, 200);
  border-radius: 1000px;
}

[type=button] {
  position: absolute;
  opacity: 0;
  width: 0;
  height: 0;
  left: -100%;
}

[type=button] + img {
  cursor: pointer;
  margin-right: 10px;
}

</style>
