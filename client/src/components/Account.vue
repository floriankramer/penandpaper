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
  <div class="container">
    <h1>Account</h1>
      <input placeholder="password" type="password" v-model="newPassword"/>
      <button v-on:click="changePassword">Change Password</button>
  </div>
</template>

<script lang="ts">

import { Component, Prop, Vue } from 'vue-property-decorator'
import Server from './server'
import eventBus from '../eventbus'

import $ from 'jquery'

@Component({
  components: {
  }
})
export default class Account extends Vue {
  userName: string = ''
  userId: number = 0
  newPassword: string = ''

  mounted () {
    $.get('/auth/self', (data: any) => {
      this.userName = data.name
      this.userId = data.id
    }).fail(() => {
      eventBus.$emit('/notification', 'Unable to query the user data.')
    })
  }

  changePassword () {
    this.hashPassword(this.userName, this.newPassword).then((hashedPassword: string) => {
      let req: any = {
        'action': 'SetPassword',
        'to-modify': this.userId,
        'new-password': hashedPassword
      }

      $.post('/auth', JSON.stringify(req), () => {
        eventBus.$emit('/notification', 'Updated your password')
      }).fail(() => {
        eventBus.$emit('/notification', 'Unable to change your password')
      })
    }).catch(() => {
      eventBus.$emit('/notification', 'Unable to change your password')
    })
    this.newPassword = ''
  }

  async hashPassword (name: string, password: string): Promise<string> {
    const passwordUtf8 = new TextEncoder().encode(name + password)
    let passwordHashBytes = await crypto.subtle.digest('SHA-256', passwordUtf8)
    let passwordArray = Array.from(new Uint8Array(passwordHashBytes))
    let passwordHex = passwordArray.map(x => ('00' + x.toString(16)).slice(-2)).join('').toUpperCase()
    return passwordHex
  }
}
</script>

<style scoped>
div.container {
  padding: 20px;
}
</style>
