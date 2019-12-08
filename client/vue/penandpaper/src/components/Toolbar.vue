<template>
  <div class="toolbar-container">
    <label>
      <input type="radio" name="current_tool">
      <img src="images/circle.png">
    </label>
    <label>
      <input type="radio" name="current_tool">
      <img src="images/line.png">
    </label>

    <button class="toolbar-align-right">Clear Tokens</button>
    <button class="toolbar-align-right">Clear Doodads</button>
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator'
import Server from './server'
import eventBus from '../eventbus'

@Component
export default class Toolbar extends Vue {
}
</script>

<style scoped>
div .toolbar-container {
  padding-right: 4px;
  padding-left: 4px;
  padding-right: 10px;
  padding-left: 10px;
  width: 100%;
  height: 100%;
}

.toolbar-align-right {
  float: right;
}

</style>
