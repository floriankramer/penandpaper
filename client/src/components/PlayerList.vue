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
  <div class="player-list">
    <table>
      <tr v-for="player in players" v-bind:key="player.uid">
        <td>{{player.name}}</td>
      </tr>
    </table>
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator'
import Server, { Player, ServerState } from './server'
import eventBus from '../eventbus'

@Component
export default class Login extends Vue {
  username: string = ''

  players: Player[] = []

  constructor () {
    super()
    eventBus.$on('/server/players/list', (players: Player[]) => {
      this.players.splice(0, this.players.length)
      players.forEach((player) => {
        this.players.push(player)
      })
    })

    eventBus.$on('/server/state', (state: ServerState) => {
      this.players.splice(0, this.players.length)
      state.players.forEach((player) => {
        this.players.push(player)
      })
    })
  }
}
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
#playerlist {
  overflow-x: hidden;
  padding-top: 7px;
}

#playerlist table thead {
  font-weight: bold;
}

#playerlist table {
  border: 0px;
}

#playerlist table td {
  padding-left: 14px;
}

#playerlist table thead td {
  padding-left: 7px;
}
</style>
