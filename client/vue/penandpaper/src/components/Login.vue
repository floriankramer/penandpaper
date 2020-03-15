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
  <div class="login-overlay">
    <div class="login-popup">
      <div class="login-centered-content">
        <span>Please enter a username:</span>
        <input autofocus="true" v-on:keyup.enter="onInputEnter" v-model="username" style="margin-right: 10px;"/>
        <button v-on:click="onOkClicked">ok</button>
      </div>
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator'
import Server from './server'

@Component
export default class Login extends Vue {
  username: string = ''

  onOkClicked (event : Event) {
    this.$store.commit('setUsername', this.username)
  }

  onInputEnter (event : Event) {
    this.$store.commit('setUsername', this.username)
  }
}
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
.login-overlay {
  position: absolute;
  width: 100%;
  height: 100%;
  left: 0px;
  top: 0px;
  background-color: rgba(71, 71, 71, 1);
}

.login-popup {
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
  text-align: center;
}

.login-centered-content {
  position: absolute;
  top: 50%;
  transform: translateY(-50%);
}

.login-centered-content input {
  margin-top: 10px;
  margin-bottom: 10px;
}
</style>
