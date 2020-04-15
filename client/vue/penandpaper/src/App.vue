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
  <div id="app">
    <CriticalError v-if="hasCriticalError">{{criticalErrorString}}</CriticalError>
    <div id="dock-container"></div>
    <Toolbar id="toolbar"></Toolbar>
    <Chat id="chat"></Chat>
    <Map id="map" class="content-area"/>
    <PlayerList id="playerlist"/>
    <Wiki id="wiki"/>
    <template v-if="noUsername">
      <Login v-bind:server="server"></Login>
    </template>
  </div>
</template>

<script lang="ts">
import { Component, Vue } from 'vue-property-decorator'
import Login from './components/Login.vue'
import Chat from './components/Chat.vue'
import Toolbar from './components/Toolbar.vue'
import Map from './components/Map.vue'
import CriticalError from './components/CriticalError.vue'
import Server from './components/server'
import PlayerList from './components/PlayerList.vue'
import Wiki from './components/Wiki.vue'
import { MutationPayload } from 'vuex'

import { DockManager } from 'dock-spawn-ts/lib/js/DockManager'
import { PanelContainer } from 'dock-spawn-ts/lib/js/PanelContainer'
import { PanelType } from 'dock-spawn-ts/lib/js/enums/PanelContainerType'

import 'dock-spawn-ts/lib/css/dock-manager.css'
import 'dock-spawn-ts/lib/css/dock-manager-style.css'
import { DockNode } from 'dock-spawn-ts/lib/js/DockNode'

@Component({
  components: {
    Login,
    CriticalError,
    Chat,
    Toolbar,
    Map,
    PlayerList,
    Wiki
  }
})
export default class App extends Vue {
  server:Server = new Server(this.$store);

  hasCriticalError:boolean = false;
  criticalErrorString:string = '';

  noUsername: boolean = true;
  isGamemaster: boolean = false;

  dockManager?: DockManager = undefined
  dockManagerDiv : HTMLElement | null = null

  toolbarPanel: PanelContainer | null = null
  wikiPanel: PanelContainer | null = null

  mounted () {
    console.log('The app has been mounted')
    this.server.setErrorHandler(this.onConnectError)
    this.server.connect()

    let app = this
    this.$store.subscribe((mutation: MutationPayload, state: any) => {
      if (mutation.type === 'setUsername') {
        if (state.username.length > 0) {
          app.noUsername = false
        }
      }
      if (mutation.type === 'setPermissions') {
        app.isGamemaster = state.permissions > 0
        if (this.dockManager !== undefined) {
          if (this.toolbarPanel !== null) {
            if (this.isGamemaster) {
              let documentNode = this.dockManager.context.model.documentManagerNode
              this.dockManager.dockUp(documentNode, this.toolbarPanel, 0.15)
            } else {
              this.toolbarPanel.close()
            }
          }
          if (this.wikiPanel !== null) {
            if (this.isGamemaster) {
              let documentNode = this.dockManager.context.model.documentManagerNode
              this.dockManager.dockFill(documentNode, this.wikiPanel)
            } else {
              this.wikiPanel.close()
            }
          }

          // Trigger a round of resizes by firing the dock library's custom resize event
          let dockSpawnResizedEvent = new CustomEvent('DockSpawnResizedEvent', { composed: true, bubbles: true })
          document.dispatchEvent(dockSpawnResizedEvent)
        }
      }
    })

    this.dockManagerDiv = document.getElementById('dock-container')
    if (this.dockManagerDiv) {
      // Initialize the dock manager
      this.dockManager = new DockManager(this.dockManagerDiv)
      this.dockManager.initialize()
      this.dockManager.resize(this.$el.clientWidth,
        this.$el.clientHeight)

      // dock the elements
      let documentNode = this.dockManager.context.model.documentManagerNode

      // The wiki
      let wikiDiv = document.getElementById('wiki')
      if (wikiDiv !== null) {
        this.wikiPanel = new PanelContainer(wikiDiv, this.dockManager)
        this.wikiPanel.setTitle('Wiki')
        if (this.isGamemaster) {
          this.dockManager.dockFill(documentNode, this.wikiPanel)
        }
      }

      // The map
      let mapDiv = document.getElementById('map')
      if (mapDiv) {
        let mapContainer = new PanelContainer(mapDiv, this.dockManager)
        mapContainer.setTitle('Map')
        this.dockManager.dockFill(documentNode, mapContainer)
      }

      // The Chat
      let chatDiv = document.getElementById('chat')
      let chatNode : DockNode | null = null
      if (chatDiv) {
        let chatContainer = new PanelContainer(chatDiv, this.dockManager)
        chatContainer.setTitle('Chat')
        chatNode = this.dockManager.dockRight(documentNode, chatContainer, 0.2)
      }
      // The PlayerList
      let playerListDiv = document.getElementById('playerlist')
      if (playerListDiv) {
        let playerListContainer = new PanelContainer(playerListDiv, this.dockManager)
        playerListContainer.setTitle('playerList')
        if (chatNode !== null) {
          this.dockManager.dockUp(chatNode, playerListContainer, 0.2)
        } else {
          this.dockManager.dockRight(documentNode, playerListContainer, 0.2)
        }
      }
      // The Toolbar
      let toolbarDiv = document.getElementById('toolbar')
      if (toolbarDiv) {
        this.toolbarPanel = new PanelContainer(toolbarDiv, this.dockManager)
        this.toolbarPanel.setTitle('Toolbar')
        this.dockManager.dockUp(documentNode, this.toolbarPanel, 0.15)
        if (!this.isGamemaster) {
          this.toolbarPanel.close()
        }
      }

      // Trigger a round of resizes by firing the dock library's custom resize event
      let dockSpawnResizedEvent = new CustomEvent('DockSpawnResizedEvent', { composed: true, bubbles: true })
      document.dispatchEvent(dockSpawnResizedEvent)
    } else {
      this.criticalErrorString = 'Unable to initalize the ui.'
    }
    window.onresize = () => {
      if (this.dockManager !== undefined && this.dockManagerDiv !== null) {
        this.dockManager.resize(this.$el.clientWidth,
          this.$el.clientHeight)
      }
    }
  }

  onConnectError () {
    this.hasCriticalError = true
    this.criticalErrorString = 'Lost connection to the server.'
  }
}
</script>

<style>

body {
  font-family: 'Avenir', Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  color: white;
  background-color: rgb(51, 51, 51);
  font-size: 13pt;
}

input {
  background-color: rgb(100, 100, 100);
  color: white;
  border: none;
  border-radius: 5px;
  padding: 3px;
}

button {
  background-color: rgb(100, 100, 100);
  color: white;
  border: none;
  border-radius: 5px;
  padding: 3px;
  padding-right: 20px;
  padding-left: 20px;
}

#app {
  top: 0px;
  left: 0px;
  bottom: 0px;
  right: 0px;
  overflow: hidden;
  position: absolute;
}

#dock-container {
  top: 0px;
  left: 0px;
  bottom: 0px;
  right: 0px;
  position: relative;
}

/** Overwrite some of the dock library's default css */
.dockspan-tab-handle-list-container {
    background-color: rgb(41, 41, 41);
}

.dockspan-tab-content {
    background-color: rgb(41, 41, 41);
}

.dockspan-tab-content > * {
    background-color: rgb(41, 41, 41);
}

.panel-titlebar-button-close {
  display: none !important;
}

.dockspan-tab-handle-close-button {
  display: none !important;
}

.panel-content {
    background-color: rgb(41, 41, 41);
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
</style>
