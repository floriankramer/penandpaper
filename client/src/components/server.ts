/**
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
 */

// This handles the connection with the server, and stores any information connected to that

import Vuex, { Store, MutationPayload } from 'vuex'

import eventBus from '../eventbus'
import * as Sim from '../simulation/simulation'
import * as B from '../simulation/building'
import PacketDispatcher from './packetdispatcher'

import BuildingServer from './buildingserver'

export class Player {
  name: string = ''
  permissions: number = 0
  uid: string = ''
}

// Used to communicate the simulation state of the server via the event bus
export class ServerState {
  tokens: Sim.Token[] = []
  lines: Sim.Line[] = []
  building: B.Building | null = null
  isStateLoaded = false
  tilesPath: string = ''
  players: Player[] = []
  pluginNames: string[] = []
}

export default class Server implements PacketDispatcher {
    socket?: WebSocket;
    store: Store<any>;

    tokens: Sim.Token[] = []
    lines: Sim.Line[] = []

    buildingServer: BuildingServer

    isStateLoaded = false

    tilesPaths: string = ''

    players: Player[] = []

    pluginNames: string[] = []

    pluginListeners: Map<string, (data: object) => void> = new Map()

    errorhandler?: () => void;

    constructor (store: Store<any>) {
      console.log('Creating a new server')
      this.store = store
      this.store.subscribe((mutation: MutationPayload, state: any) => {
        this.onmutation(mutation, state)
      })
      this.buildingServer = new BuildingServer(this)
      eventBus.$on('/chat/send', (data:string) => { this.sendChat(data) })
      eventBus.$on('/client/token/create', (data:Sim.Point) => { this.clientCreateToken(data) })
      eventBus.$on('/client/token/move', (data: Sim.TokenMoveOrder) => { this.onClientMoveToken(data) })
      eventBus.$on('/client/token/clear', () => { this.clientClearTokens() })
      eventBus.$on('/client/token/toggle_foe', (data: Sim.Token) => { this.onClientTokenToggleFoe(data) })
      eventBus.$on('/client/token/delete', (data: Sim.Token) => { this.onClientDeleteToken(data) })
      eventBus.$on('/client/token/rename', (data: Sim.Token, newName: string) => { this.onClientRenameToken(data, newName) })
      eventBus.$on('/client/line/create', (data: Sim.Line) => { this.onClientCreateLine(data) })
      eventBus.$on('/client/line/clear', () => { this.onClientClearLines() })
      eventBus.$on('/client/plugin/send', (name: string, data: any) => {
        this.onClientPluginSend(name, data)
      })
      eventBus.$on('/client/plugin/listen', (name: string, handler: (data: object) => void) => {
        this.pluginListeners.set(name, handler)
      })
      eventBus.$on('/server/request_state', () => { this.onStateRequest() })
    }

    connect () {
      console.log('Connecting to the ws server.')
      let port = ':8082'
      this.socket = new WebSocket('wss://' + window.location.hostname + port)

      // Wrap the callbacks into anonymous functions to ensure they are called
      // with the correct object as 'this'
      var server = this
      this.socket.onmessage = function (msg:MessageEvent) {
        server.onmessage(msg)
      }
      this.socket.onerror = function (err:Event) {
        server.onerror(err)
      }
      this.socket.onopen = function (msg:Event) {
        server.onopen(msg)
      }
      this.socket.onclose = function (msg:Event) {
        server.onclose(msg)
      }
    }

    onmessage (msg: MessageEvent) {
      let packet = JSON.parse(msg.data)
      let type = packet['type']
      let data = packet['data']
      if (type === 'Session') {
        this.onServerSession(data)
      } else if (type === 'Chat') {
        this.onchat(data)
      } else if (type === 'Init') {
        this.onServerInit(data)
      } else if (type === 'CreateToken') {
        this.onServerCreateToken(data)
      } else if (type === 'ClearTokens') {
        this.serverClearTokens()
      } else if (type === 'MoveToken') {
        this.onServerMoveToken(data)
      } else if (type === 'RenameToken') {
        this.onServerRenameToken(data)
      } else if (type === 'DeleteToken') {
        this.onServerDeleteToken(data)
      } else if (type === 'CreateDoodadLine') {
        this.onServerCreateLine(data)
      } else if (type === 'ClearDoodads') {
        this.onServerClearDoodads()
      } else if (type === 'TokenToggleFoe') {
        this.onServerTokenToggleFoe(data)
      } else if (type === 'ToggleDoor') {
        this.onServerToggleDoor(data)
      } else if (type === 'SetTiles') {
        this.onServerSetTiles(data)
      } else if (type === 'ClearTiles') {
        this.onServerClearTiles(data)
      } else if (type === 'PlayerList') {
        this.onServerPlayerList(data)
      } else if (type === 'PlayAudio') {
        this.onServerPlayAudio(data)
      } else if (type === 'StreamAudio') {
        this.onServerStreamAudio(data)
      } else if (type === 'StopAudioStreams') {
        this.onServerStopAudioStreams(data)
      } else if (this.buildingServer.onmessage(type, data)) {
        // the message was handled by the buildingServer
      } else if (this.pluginListeners.has(type)) {
        let h = this.pluginListeners.get(type)
        if (h !== undefined) {
          h(data)
        }
      }
    }

    onchat (data : any) {
      eventBus.$emit('/chat/message', { sender: data['sender'], message: data['message'] })
    }

    onServerInit (data: any) {
      this.isStateLoaded = true
      for (let rawToken of data.tokens) {
        let token = new Sim.Token()
        token.x = rawToken.x
        token.y = rawToken.y
        token.id = rawToken.id
        token.isFoe = rawToken.foe
        token.color.r = rawToken.r
        token.color.g = rawToken.g
        token.color.b = rawToken.b
        token.rotation = rawToken.rotation
        token.name = rawToken.name
        this.tokens.push(token)
      }
      for (let rawLine of data.doodads) {
        let line = new Sim.Line()
        line.start.x = rawLine.sx
        line.start.y = rawLine.sy
        line.stop.x = rawLine.ex
        line.stop.y = rawLine.ey
        this.lines.push(line)
      }
      this.pluginNames = data.plugins

      this.buildingServer.onServerInit(data)
      this.tilesPaths = data['tiles']
      // Broadcast our initialized state
      this.onStateRequest()
    }

    sendChat (text : string) {
      let packet = {
        type: 'Chat',
        data: {
          sender: this.store.state.username,
          message: text
        }
      }
      this.send(JSON.stringify(packet))
    }

    onServerCreateToken (data: any) {
      let token = new Sim.Token()
      token.x = data.x
      token.y = data.y
      token.radius = 0.25
      token.id = data.id
      token.isFoe = data.foe
      token.color = new Sim.Color(data.r, data.g, data.b)
      token.name = data.name

      eventBus.$emit('/server/token/create', token)
      this.tokens.push(token)
    }

    clientCreateToken (pos: Sim.Point) {
      let packet = {
        type: 'CreateToken',
        data: {
          x: pos.x,
          y: pos.y
        }
      }
      this.send(JSON.stringify(packet))
    }

    clientClearTokens () {
      let packet = {
        type: 'ClearTokens',
        data: { }
      }
      this.send(JSON.stringify(packet))
    }

    serverClearTokens () {
      eventBus.$emit('/server/token/clear')
    }

    onClientMoveToken (move: Sim.TokenMoveOrder) {
      let packet = {
        type: 'MoveToken',
        data: {
          'x': move.x,
          'y': move.y,
          'id': move.token.id,
          'rotation': move.rotation
        }
      }
      this.send(JSON.stringify(packet))
    }

    onServerMoveToken (data: any) {
      let token = this.findTokenById(data.id)
      if (token !== undefined) {
        let move = new Sim.TokenMoveOrder()
        move.x = data.x
        move.y = data.y
        move.rotation = data.rotation
        move.token = token

        // Apply the move to our state
        move.token.x = move.x
        move.token.y = move.y
        move.token.rotation = move.rotation

        eventBus.$emit('/server/token/move', move)
      } else {
        console.log('Got a move order for a token with id ', data.id, ' but was unable to find such a token.')
      }
    }

    onClientDeleteToken (data: Sim.Token) {
      let packet = {
        type: 'DeleteToken',
        data: {
          'id': data.id
        }
      }
      this.send(JSON.stringify(packet))
    }

    onServerDeleteToken (data: any) {
      let token = this.findTokenById(data.id)
      if (token !== undefined) {
        eventBus.$emit('/server/token/delete', token)
        let pos = this.tokens.findIndex((t: Sim.Token) => { return t.id === data.id })
        this.tokens.splice(pos, 1)
      } else {
        console.log('Received a DeleteToken packet with data', data, 'but didn\'t find a token with a matching id')
      }
    }

    onClientRenameToken (data: Sim.Token, newName: string) {
      let packet = {
        type: 'RenameToken',
        data: {
          'id': data.id,
          'new_name': newName
        }
      }
      this.send(JSON.stringify(packet))
    }

    onServerRenameToken (data: any) {
      let token = this.findTokenById(data.id)
      if (token !== undefined) {
        token.name = data.new_name
        eventBus.$emit('/server/token/rename', token)
      } else {
        console.log('Received a RenameToken packet with data', data, 'but didn\'t find a token with a matching id')
      }
    }

    onClientCreateLine (line: Sim.Line) {
      let packet = {
        type: 'CreateDoodadLine',
        data: {
          'sx': line.start.x,
          'sy': line.start.y,
          'ex': line.stop.x,
          'ey': line.stop.y
        }
      }
      this.send(JSON.stringify(packet))
    }

    onServerCreateLine (data: any) {
      let line = new Sim.Line()
      line.start.x = data.sx
      line.start.y = data.sy
      line.stop.x = data.ex
      line.stop.y = data.ey
      line.id = data.id
      eventBus.$emit('/server/line/create', line)
      this.lines.push(line)
    }

    onClientClearLines () {
      let packet = {
        type: 'ClearDoodads',
        data: { }
      }
      this.send(JSON.stringify(packet))
    }

    onClientPluginSend (name: string, data: any) {
      let packet = {
        type: name,
        data: data
      }
      this.send(JSON.stringify(packet))
    }

    onServerToggleDoor (data: any) {
      eventBus.$emit('/server/building/toggle_door', data.ids)
    }

    onServerClearDoodads () {
      eventBus.$emit('/server/line/clear')
      this.lines.splice(0, this.lines.length)
    }

    onStateRequest () {
      let state = new ServerState()
      state.tokens = this.tokens
      state.lines = this.lines
      state.building = this.buildingServer.building
      state.isStateLoaded = this.isStateLoaded
      state.tilesPath = this.tilesPaths
      state.players = this.players
      state.pluginNames = this.pluginNames
      eventBus.$emit('/server/state', state)
    }

    onServerTokenToggleFoe (data: any) {
      let token = this.findTokenById(data.id)
      if (token !== undefined) {
        token.isFoe = !token.isFoe
        eventBus.$emit('/server/token/toggle_foe', token)
      } else {
        console.log('The server requested an is foe change, but no token with the given id exists: ', data)
      }
    }

    onClientTokenToggleFoe (token: Sim.Token) {
      let packet = {
        type: 'TokenToggleFoe',
        data: {
          'id': token.id
        }
      }
      this.send(JSON.stringify(packet))
    }

    onServerSession (data: any) {
      let permissions = parseInt(data['permissions'])
      this.store.commit('setUsername', data['name'])
      this.store.commit('setPermissions', permissions)
      eventBus.$emit('/server/is_gm', permissions > 0)
    }

    onServerSetTiles (data: any) {
      eventBus.$emit('/server/tiles/set', data['path'])
    }

    onServerClearTiles (data: any) {
      eventBus.$emit('/server/tiles/clear')
    }

    onerror (err:Event) {
      err.stopPropagation()
      console.log('Unable to connect to the server')
      console.log('errorhandler ' + this.errorhandler)
      if (this.errorhandler) {
        this.errorhandler()
      }
    }

    onopen (ev:Event) {
      console.log('Connected to the server')
      // Send the session init
      let packet = {
        type: 'InitSession',
        data: { }
      }
      this.send(JSON.stringify(packet))
    }

    onclose (ev:Event) {
      console.log('The connection to the server was closed')
      if (this.errorhandler) {
        this.errorhandler()
      }
    }

    setErrorHandler (handler: () => void) {
      this.errorhandler = handler
      console.log('The errorhandler is ' + this.errorhandler)
    }

    send (data : string) {
      if (this.socket) {
        this.socket.send(data)
      }
    }

    onmutation (mutation: MutationPayload, state: any) {
      if (mutation.type === 'setUsername') {
        if (state.username.length > 0) {
          let packet = {
            type: 'SetUsername',

            data: { name: state.username }
          }
          this.send(JSON.stringify(packet))
        }
      }
    }

    onServerPlayerList (data: any[]) {
      this.players.splice(0, this.players.length)
      data.forEach(rawp => {
        let p = rawp as Player
        this.players.push(p)
      })
      eventBus.$emit('/server/players/list', this.players)
    }

    onServerPlayAudio (data: any) {
      var src: string = data.src
      let volume = 1
      if (data.volume !== undefined) {
        volume = data.volume
      }
      eventBus.$emit('/audio/play', src, volume)
    }

    onServerStreamAudio (data: any) {
      var src: string = data.src
      let volume = 1
      if (data.volume !== undefined) {
        volume = data.volume
      }
      eventBus.$emit('/audio/stream', src, volume)
    }

    onServerStopAudioStreams (data: any) {
      eventBus.$emit('/audio/stop-streams')
    }

    findTokenById (id: number) : Sim.Token | undefined {
      return this.tokens.find((token: Sim.Token) => { return token.id === id })
    }
}
