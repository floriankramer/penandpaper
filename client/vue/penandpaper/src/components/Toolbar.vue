<template>
  <div class="toolbar-container">
    <label>
      <input type="radio" name="current_tool" value='view' v-model='currentTool' v-on:change='onToolView'>
      <img src="images/eye.svg" width="38" height="38">
    </label>
    <label>
      <input type="radio" name="current_tool" value='token' v-model='currentTool' v-on:change='onToolToken'>
      <img src="images/circle.png" width="38" height="38">
    </label>
    <label>
      <input type="radio" name="current_tool" value='line' v-model='currentTool' v-on:change='onToolLine'>
      <img src="images/line.png" width="38" height="38">
    </label>
    <label>
      <input type="radio" name="current_tool" value='room' v-model='currentTool' v-on:change='onToolRoom'>
      <img src="images/line.png" width="38" height="38">
    </label>
    <label>
      <input type="radio" name="current_tool" value='door' v-model='currentTool' v-on:change='onToolDoor'>
      <img src="images/line.png" width="38" height="38">
    </label>

    <button class="toolbar-align-right toolbar-center-verticaly" v-on:click="saveBuilding">Save Building</button>
    <input type="file" class="toolbar-align-right toolbar-center-verticaly" v-on:change="loadBuilding" accept=".json"/>
    <button class="toolbar-align-right toolbar-center-verticaly" v-on:click="clearTokens">Clear Tokens</button>
    <button class="toolbar-align-right toolbar-margin-right toolbar-center-verticaly" v-on:click="clearDoodads">Clear Doodads</button>
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator'
import Server from './server'
import eventBus from '../eventbus'

@Component
export default class Toolbar extends Vue {
  currentTool: string = 'view'

  clearTokens () {
    eventBus.$emit('/client/token/clear')
  }

  clearDoodads () {
    eventBus.$emit('/client/line/clear')
  }

  onToolView () {
    eventBus.$emit('/tools/select_tool', 'view')
  }

  onToolToken () {
    eventBus.$emit('/tools/select_tool', 'token')
  }

  onToolLine () {
    eventBus.$emit('/tools/select_tool', 'line')
  }

  onToolRoom () {
    eventBus.$emit('/tools/select_tool', 'room')
  }

  onToolDoor () {
    eventBus.$emit('/tools/select_tool', 'door')
  }

  saveBuilding () {
    eventBus.$emit('/client/building/save')
  }

  loadBuilding (e : Event) {
    if (e.target !== null && e.target instanceof HTMLInputElement) {
      let input: HTMLInputElement = e.target as HTMLInputElement
      if (input.files !== null && input.files.length > 0) {
        eventBus.$emit('/client/building/load', input.files.item(0))
      }
    }
  }
}
</script>

<style scoped>
div .toolbar-container {
  position: absolute;
  left: 10px;
  right: 10px;
  top: 0px;
  bottom: 0px;
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

</style>
