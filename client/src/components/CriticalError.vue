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
  <div class="error-overlay">
    <div class="error-popup">
      <div class="error-centered-content">
        <span>A critical error occured: </span>
        <slot></slot>
        Page will reload in {{reloadIn}}s.
      </div>
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator'
import Server from './server'

@Component
export default class CriticalError extends Vue {
  reloadIn: number = 4

  mounted () {
    this.countdown()
  }

  countdown () {
    this.reloadIn -= 1
    if (this.reloadIn === 0) {
      location.reload()
    } else {
      setTimeout(this.countdown, 1000)
    }
  }
}
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
.error-overlay {
  position: absolute;
  width: 100%;
  height: 100%;
  left: 0px;
  top: 0px;
  background-color: rgba(70, 70, 70, 0.5);
}

.error-popup {
  position: absolute;
  width: 300px;
  height: 200px;
  margin-left: -150px;
  margin-top: -100px;
  left: 50%;
  top: 50%;
  background-color: rgb(51, 51, 51);
  border-radius: 10px;
  padding-top: auto;
  padding-bottom: auto;
}

.error-centered-content {
  position: absolute;
  top: 50%;
  transform: translateY(-50%);
  text-align: center;
}
</style>
