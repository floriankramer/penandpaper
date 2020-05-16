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
    <Menu v-bind:menus="menus" v-on:selected="onMenuSelected" id="menu"/>
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
import Menu, { MenuGroup, MenuItem, MenuKeyShortcut } from './components/Menu.vue'

import { MutationPayload } from 'vuex'

import { DockManager } from 'dock-spawn-ts/lib/js/DockManager'
import { PanelContainer } from 'dock-spawn-ts/lib/js/PanelContainer'
import { PanelType } from 'dock-spawn-ts/lib/js/enums/PanelContainerType'

import 'dock-spawn-ts/lib/css/dock-manager.css'
import 'dock-spawn-ts/lib/css/dock-manager-style.css'
import { DockNode } from 'dock-spawn-ts/lib/js/DockNode'

import GlobalSettings from './global'

import $ from 'jquery'
import eventbus from './eventbus'

@Component({
  components: {
    Login,
    CriticalError,
    Chat,
    Toolbar,
    Map,
    PlayerList,
    Wiki,
    Menu
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
  mapPanel: PanelContainer | null = null
  chatPanel: PanelContainer | null = null
  playerListPanel: PanelContainer | null = null

  mapDockNode: DockNode | null = null

  menus: MenuGroup[] = [
    {
      text: 'window',
      items: [
        {
          id: 'window-wiki',
          text: 'wiki'
        },
        {
          id: 'window-map',
          text: 'map'
        },
        {
          id: 'window-chat',
          text: 'chat'
        },
        {
          id: 'window-toolbar',
          text: 'toolbar'
        },
        {
          id: 'window-playerlist',
          text: 'playerlist'
        }
      ]
    },
    {
      text: 'view',
      items: [
        {
          id: 'view-colorscheme',
          text: 'darker mode'
        },
        {
          id: 'view-wikionly',
          text: 'wiki only'
        }
      ]
    },
    {
      text: 'wiki',
      items: [
        {
          id: 'wiki-save',
          text: 'save [ctrl+s]',
          shortcut: {
            key: 's',
            ctrl: true
          }
        },
        {
          id: 'wiki-quickcreate',
          text: 'quickcreate [alt+z]',
          shortcut: {
            key: 'z',
            alt: true
          }
        }
      ]
    }
  ]

  mounted () {
    console.log('The app has been mounted')
    this.server.setErrorHandler(this.onConnectError)
    this.server.connect()

    let app = this

    this.dockManagerDiv = document.getElementById('dock-container')
    if (this.dockManagerDiv) {
      // Initialize the dock manager
      this.dockManager = new DockManager(this.dockManagerDiv)
      this.dockManager.initialize()
      this.dockManager.resize(this.$el.clientWidth,
        this.$el.clientHeight - 25)

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
        this.mapPanel = new PanelContainer(mapDiv, this.dockManager)
        this.mapPanel.setTitle('Map')
        this.mapDockNode = this.dockManager.dockFill(documentNode, this.mapPanel)
      }

      // The Chat
      let chatDiv = document.getElementById('chat')
      let chatNode : DockNode | null = null
      if (chatDiv) {
        this.chatPanel = new PanelContainer(chatDiv, this.dockManager)
        this.chatPanel.setTitle('Chat')
        chatNode = this.dockManager.dockRight(documentNode, this.chatPanel, 0.2)
      }
      // The PlayerList
      let playerListDiv = document.getElementById('playerlist')
      if (playerListDiv) {
        this.playerListPanel = new PanelContainer(playerListDiv, this.dockManager)
        this.playerListPanel.setTitle('playerList')
        if (chatNode !== null) {
          this.dockManager.dockUp(chatNode, this.playerListPanel, 0.2)
        } else {
          this.dockManager.dockRight(documentNode, this.playerListPanel, 0.2)
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
          this.$el.clientHeight - 25)
      }
    }

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
              if (this.mapDockNode !== null) {
                this.dockManager.dockUp(this.mapDockNode, this.toolbarPanel, 0.15)
              } else {
                this.dockManager.dockUp(documentNode, this.toolbarPanel, 0.15)
              }
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

    // Dark mode
    $('body').addClass('light-mode')
  }

  toggleDarkerMode () {
    GlobalSettings.darkMode = !GlobalSettings.darkMode
    if (GlobalSettings.darkMode) {
      $('body').removeClass('light-mode').addClass('dark-mode')
    } else {
      $('body').addClass('light-mode').removeClass('dark-mode')
    }
    eventbus.$emit('/client/colorscheme/changed')
  }

  onConnectError () {
    this.hasCriticalError = true
    this.criticalErrorString = 'Lost connection to the server.'
  }

  togglePanel (panel: PanelContainer) {
    if (this.dockManager !== undefined) {
      if (this.dockManager.getVisiblePanels().includes(panel)) {
        panel.close()
      } else {
        this.dockManager.dockFill(this.dockManager.context.model.documentManagerNode, panel)
      }
    }
  }

  showWikiOnly () {
    if (this.dockManager === undefined) {
      return
    }
    let docnode = this.dockManager.context.model.documentManagerNode
    if (this.wikiPanel !== null) {
      this.dockManager.dockFill(docnode, this.wikiPanel)
    }
    if (this.mapPanel !== null) {
      this.mapPanel.close()
    }
    if (this.chatPanel !== null) {
      this.chatPanel.close()
    }
    if (this.playerListPanel !== null) {
      this.playerListPanel.close()
    }
    if (this.toolbarPanel !== null) {
      this.toolbarPanel.close()
    }
  }

  onMenuSelected (id: string) {
    console.log(id)
    if (id === 'window-wiki') {
      if (this.isGamemaster) {
        if (this.wikiPanel !== null) {
          this.togglePanel(this.wikiPanel)
        }
      }
    } else if (id === 'window-map') {
      if (this.mapPanel !== null) {
        this.togglePanel(this.mapPanel)
      }
    } else if (id === 'window-chat') {
      if (this.chatPanel !== null) {
        this.togglePanel(this.chatPanel)
      }
    } else if (id === 'window-playerlist') {
      if (this.playerListPanel !== null) {
        this.togglePanel(this.playerListPanel)
      }
    } else if (id === 'window-toolbar') {
      if (this.toolbarPanel !== null) {
        this.togglePanel(this.toolbarPanel)
      }
    } else if (id === 'view-colorscheme') {
      this.toggleDarkerMode()
    } else if (id === 'view-wikionly') {
      this.showWikiOnly()
    } else {
      eventbus.$emit('/menu/' + id)
    }
  }
}
</script>

<style>
/**
The original colors where:
background: background-color: rgb(41, 41, 41);
font: white

After looking at the material design guidelines, we use
background: #1a1a1a
color: #afafaf
 */
body {
  font-family: 'Avenir', Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  font-size: 13pt;
}

body.dark-mode {
  color: #afafaf;
  background-color: #1a1a1a;
}

body.dark-mode .panel-titlebar {
  background-color: #1a1a1a;
}

body.dark-mode .panel-content {
    background-color: #1a1a1a
}

body.dark-mode .dockspan-tab-handle-list-container {
    background-color: #1a1a1a
}

body.dark-mode .dockspan-tab-content {
    background-color: #1a1a1a
}

body.dark-mode .dockspan-tab-content > * {
    background-color: #1a1a1a
}

body.light-mode {
  color: #eeeeee;
  background-color: #333333;
}

body.light-mode .panel-titlebar {
  background-color: #333333;
}

body.light-mode .panel-content {
    background-color: #333333
}
body.light-mode .dockspan-tab-handle-list-container {
    background-color: #333333
}

body.light-mode .dockspan-tab-content {
    background-color: #333333
}

body.light-mode .dockspan-tab-content > * {
    background-color: #333333
}

.panel-titlebar-active {
  background-color: #008749;
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

#menu {
  position: absolute;
  top: 0px;
  left: 0px;
  right: 0px;
  height: 25px;
}

#dock-container {
  top: 25px;
  left: 0px;
  bottom: 0px;
  right: 0px;
  position: relative;
}

/** Overwrite some of the dock library's default css */

.panel-titlebar-button-close {
  display: none !important;
}

.dockspan-tab-handle-close-button {
  display: none !important;
}

a {
  /**color: rgb(128, 255, 64);**/
  color: #61af3a;
  text-decoration: none;
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
