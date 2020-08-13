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
  <div v-show="isVisible" v-bind:class="{ 'notification-popup-hidden': offScreen }" class="notification-popup">
    {{text}}
  </div>
</div>
</template>

<script lang="ts">
import { Component, Prop, Vue, Watch } from 'vue-property-decorator'

@Component
export default class Notification extends Vue {
  @Prop()
  text: string = ''

  isVisible = false
  offScreen = true

  timeoutHandle: number | null = null
  hideTimeoutHandle: number | null = null

  @Watch('text')
  onTextChanged (v: string, old: string) {
    if (v.length === 0) {
      this.hide()
    } else {
      this.show()
    }
  }

  show () {
    if (this.timeoutHandle !== null) {
      clearTimeout(this.timeoutHandle)
    }
    if (this.hideTimeoutHandle !== null) {
      clearTimeout(this.hideTimeoutHandle)
      this.hideTimeoutHandle = null
    }

    this.isVisible = true
    this.offScreen = false

    this.timeoutHandle = setTimeout(() => {
      this.hide()
    }, 1500)
  }

  hide () {
    this.hideTimeoutHandle = setTimeout(() => {
      this.hideTimeoutHandle = null
      this.isVisible = false
    }, 1000)
    this.timeoutHandle = null
    this.offScreen = true
  }
}
</script>

<style scoped>
div.notification-popup {
  position: absolute;
  max-width: 200px;
  left: 50%;
  top: 100px;
  z-index: 3000;
  box-shadow: black 0px 0px 5px;
  transform: translateX(-50%);
  background: black;
  border-radius: 13px;
  color: white;
  padding: 15px;
  transition: top 1s;
}

div.notification-popup-hidden {
  top: -200px !important;
}
</style>
