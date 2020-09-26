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
    <Notification ref="notification" v-bind:text="notificationText" />
    <Menu v-bind:menus="menus" v-on:selected="onMenuSelected" id="menu"/>
    <CriticalError v-if="hasCriticalError">{{criticalErrorString}}</CriticalError>
    <div id="dock-container"></div>
    <Toolbar id="toolbar"></Toolbar>
    <Chat id="chat"></Chat>
    <WorldMap id="map" class="content-area"/>
    <PlayerList id="playerlist"/>
    <Wiki id="wiki"/>
    <UserManager id="user-manager"/>
    <Account id="account"/>
    <div v-for="plugin in plugins" v-bind:key="plugin.idx">
      <style>{{plugin.css}}</style>
      <div v-html="plugin.html" v-bind:id="'container-' + plugin.name"></div>
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Vue } from 'vue-property-decorator'
import Chat from './components/Chat.vue'
import Toolbar from './components/Toolbar.vue'
import WorldMap from './components/WorldMap.vue'
import CriticalError from './components/CriticalError.vue'
import Server, { ServerState } from './components/server'
import PlayerList from './components/PlayerList.vue'
import Menu, { MenuGroup, MenuItem, MenuKeyShortcut } from './components/Menu.vue'
import Notification from './components/Notification.vue'
import AudioServer from './audio/AudioServer'

import Wiki from './components/Wiki.vue'
import UserManager from './components/UserManager.vue'
import Account from './components/Account.vue'

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

// An instance of this class is given to every plugin for interfacing
// with the host application.
class PluginHost {
  // The plugins name
  name: string = ''

  sendPacket (name: string, data: any) {
    eventbus.$emit('/client/plugin/send', this.name + '::' + name, data)
  }

  listenToPacket (name: string, handler: (data: object) => void) {
    eventbus.$emit('/client/plugin/listen', this.name + '::' + name, handler)
  }
}

class Plugin {
  idx: number = 0
  html: string = ''
  css: string = ''
  js: string = ''
  name: string = ''

  instance: any = {}
}

@Component({
  components: {
    CriticalError,
    Chat,
    Toolbar,
    WorldMap,
    PlayerList,
    Wiki,
    Menu,
    Notification,
    UserManager,
    Account
  }
})
export default class App extends Vue {
  server: Server = new Server(this.$store);

  hasCriticalError: boolean = false;
  criticalErrorString: string = '';

  isGamemaster: boolean = false;

  dockManager?: DockManager = undefined
  dockManagerDiv : HTMLElement | null = null

  toolbarPanel: PanelContainer | null = null
  wikiPanel: PanelContainer | null = null
  userManagerPanel: PanelContainer | null = null
  accountPanel: PanelContainer | null = null
  mapPanel: PanelContainer | null = null
  chatPanel: PanelContainer | null = null
  playerListPanel: PanelContainer | null = null

  pluginPanels: Map<string, PanelContainer> = new Map();

  mapDockNode: DockNode | null = null

  notificationText: string = ''

  audioServer: AudioServer = new AudioServer()

  plugins: Plugin[] = []
  pluginMenu: MenuGroup = {
    text: 'plugins',
    items: []
  }

  audioMenu: MenuGroup = {
    text: 'audio',
    items: [
      {
        id: 'audio-mute',
        text: 'mute'
      },
      {
        id: 'audio-unmute',
        text: 'unmute'
      }
    ]
  }

  accountMenu: MenuGroup = {
    text: 'account',
    items: [
      {
        id: 'account-logout',
        text: 'logout'
      }
    ]
  }

  windowMenu: MenuGroup = {
    text: 'window',
    items: [
      {
        id: 'window-map',
        text: 'map'
      },
      {
        id: 'window-chat',
        text: 'chat'
      },
      {
        id: 'window-playerlist',
        text: 'playerlist'
      },
      {
        id: 'window-account',
        text: 'account'
      }
    ]
  }

  windowMenuGM : MenuItem[] = [
    {
      id: 'window-wiki',
      text: 'wiki'
    },
    {
      id: 'window-user-manager',
      text: 'user manager'
    },
    {
      id: 'window-toolbar',
      text: 'toolbar'
    }
  ]

  viewMenu: MenuGroup = {
    text: 'view',
    items: [
      {
        id: 'view-colorscheme',
        text: 'darker mode'
      }
    ]
  }

  viewMenuGM: MenuItem[] = [
    {
      id: 'view-wikionly',
      text: 'wiki only'
    }
  ]

  wikiMenu: MenuGroup = {
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

  // The non gm menu
  menus: MenuGroup[] = [
    this.windowMenu,
    this.viewMenu,
    this.audioMenu,
    this.pluginMenu,
    this.accountMenu
  ]

  mounted () {
    eventbus.$on('/server/state', this.onServerState)

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

      // The user manager
      let userManagerDiv = document.getElementById('user-manager')
      if (userManagerDiv !== null) {
        this.userManagerPanel = new PanelContainer(userManagerDiv, this.dockManager)
        this.userManagerPanel.setTitle('User Manager')
      }

      // The account
      let accountDiv = document.getElementById('account')
      if (accountDiv !== null) {
        this.accountPanel = new PanelContainer(accountDiv, this.dockManager)
        this.accountPanel.setTitle('Account')
        this.dockManager.dockFill(documentNode, this.accountPanel)
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
      if (mutation.type === 'setPermissions') {
        app.isGamemaster = state.permissions > 0
        this.onGameMasterChange()
      }
    })

    // Dark mode
    $('body').addClass('light-mode')

    eventbus.$on('/notification', (text: string) => {
      this.notificationText = text
      let n = this.$refs.notification as Notification
      n.show()
    })

    this.onGameMasterChange()

    $.get('/auth/self', (user: any) => {
      console.log('App.vue: /auth/self: ', user, user.permissions.length)
      if (user.permissions.length > 0 && this.dockManager !== undefined && this.userManagerPanel !== null) {
        let documentNode = this.dockManager.context.model.documentManagerNode
        this.dockManager.dockFill(documentNode, this.userManagerPanel)
      }
    })
  }

  onServerState (state: ServerState) {
    // Load plugins
    state.pluginNames.forEach((name) => {
      this.loadPlugin(name)
    })
  }

  loadPlugin (name: string) {
    let onfail = (a : any, error: string) => {
      eventbus.$emit('/notification', 'Unable to load plugin ' + name)
      console.log('Unable to load plugin ' + name + ' : ' + error)
    }

    // load the html
    let plugin = new Plugin()
    plugin.name = name
    $.get('/plugin/' + name + '/html', (data) => {
      plugin.html = data
      $.get('/plugin/' + name + '/js', (data) => {
        plugin.js = data
        plugin.idx = this.plugins.length
        // load the css
        let css = document.createElement('link')
        css.rel = 'stylesheet'
        css.href = '/plugin/' + name + '/css'
        document.head.appendChild(css)

        // Add the html containers
        this.plugins.push(plugin)

        // Create a dock from the plugin
        this.$nextTick(() => {
          // Run the js. Given the js code is loaded from the filesystem on
          // the server, this is equivalent to, but more conventient than
          // creating a new script tag. That justifies the use of new Function
          // here.
          // eslint-disable-next-line
          let func = new Function(plugin.js = '\nreturn ' + plugin.name)
          // Call the function, which will return the plugin instance
          plugin.instance = func()
          let host = new PluginHost()
          host.name = plugin.name
          if (plugin.instance.init !== undefined) {
            plugin.instance.init(host)
          } else {
            console.log('The plugin ' + plugin.name + ' does not define an init function: ' + plugin)
          }

          this.createPluginDock(plugin)
          this.updatePluginMenu()
        })
      }).fail(onfail)
    }).fail(onfail)
  }

  createPluginDock (plugin: Plugin) {
    if (this.dockManager !== undefined) {
      let documentNode = this.dockManager.context.model.documentManagerNode

      let pluginDiv = document.getElementById('container-' + plugin.name)
      if (pluginDiv) {
        let pluginPanel = new PanelContainer(pluginDiv, this.dockManager)
        pluginPanel.setTitle(plugin.name)
        this.pluginPanels.set(plugin.name, pluginPanel)
      } else {
        console.log('App.createPluginDock: unable to find the plugin ' +
                    'container for ' + plugin.name)
      }
    }
  }

  onGameMasterChange () {
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
          if (this.toolbarPanel.state !== undefined) {
            this.toolbarPanel.close()
          }
        }
      }
      if (this.wikiPanel !== null) {
        if (this.isGamemaster) {
          let documentNode = this.dockManager.context.model.documentManagerNode
          this.dockManager.dockFill(documentNode, this.wikiPanel)
        } else {
          if (this.wikiPanel.state !== undefined) {
            this.wikiPanel.close()
          }
        }
      }

      // Trigger a round of resizes by firing the dock library's custom resize event
      let dockSpawnResizedEvent = new CustomEvent('DockSpawnResizedEvent', { composed: true, bubbles: true })
      document.dispatchEvent(dockSpawnResizedEvent)
    }

    // replace the menu
    if (this.isGamemaster) {
      this.menus = [
        {
          text: this.windowMenu.text,
          items: this.windowMenu.items.concat(this.windowMenuGM)
        },
        {
          text: this.viewMenu.text,
          items: this.viewMenu.items.concat(this.viewMenuGM)
        },
        this.wikiMenu,
        this.audioMenu,
        this.pluginMenu,
        this.accountMenu
      ]
    } else {
      this.menus = [
        this.windowMenu,
        this.viewMenu,
        this.audioMenu,
        this.pluginMenu,
        this.accountMenu
      ]
    }
  }

  updatePluginMenu () {
    this.pluginMenu.items = []
    this.plugins.forEach((plugin: Plugin) => {
      this.pluginMenu.items.push({
        id: 'plugin-' + plugin.name,
        text: plugin.name
      })
    })
    this.pluginMenu.items.sort((a: MenuItem, b: MenuItem) => {
      if (a.text < b.text) {
        return -1
      } else if (a.text === b.text) {
        return 0
      } else {
        return 1
      }
    })
    for (let i = 0; i < this.menus.length; ++i) {
      if (this.menus[i] === this.pluginMenu) {
        // Inform vue of the change to the array
        this.menus.splice(i)
        this.menus.push(this.pluginMenu)
        break
      }
    }
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
    if (this.userManagerPanel !== null) {
      this.userManagerPanel.close()
    }
    if (this.accountPanel !== null) {
      this.accountPanel.close()
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
    } else if (id === 'window-user-manager') {
      if (this.isGamemaster) {
        if (this.userManagerPanel !== null) {
          this.togglePanel(this.userManagerPanel)
        }
      }
    } else if (id === 'window-account') {
      if (this.accountPanel !== null) {
        this.togglePanel(this.accountPanel)
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
    } else if (id.startsWith('plugin-')) {
      let name = id.substr('plugin-'.length)
      let panel = this.pluginPanels.get(name)
      if (panel !== undefined) {
        this.togglePanel(panel)
      }
    } else if (id === 'audio-mute') {
      eventbus.$emit('/audio/mute', true)
    } else if (id === 'audio-unmute') {
      eventbus.$emit('/audio/mute', false)
    } else if (id === 'account-logout') {
      $.get('/auth/logout', () => {
        location.reload()
      })
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

/** Scroll bar styling */
body.light-mode {
  scrollbar-width: thin;
  scrollbar-color: #2b4c4c #333;
}

body.dark-mode {
  scrollbar-width: thin;
  scrollbar-color: #2b4c4c #111;
}

body.light-mode ::-webkit-scrollbar-track {
    background-color: #333;
}

body.light-mode ::-webkit-scrollbar {
    background-color: #333;
}

body.light-mode ::-webkit-scrollbar-thumb {
    background-color: #2b4c4c;
}

body.light-mode ::-webkit-scrollbar-corner {
  background: #333;
}

body.dark-mode ::-webkit-scrollbar-track {
    background-color: #111;
}

body.dark-mode ::-webkit-scrollbar {
    background-color: #111;
}

body.dark-mode ::-webkit-scrollbar-thumb {
    background-color: #2b4c4c;
}

body.dark-mode ::-webkit-scrollbar-corner {
  background: #111;
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
