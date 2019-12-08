// This handles the connection with the server, and stores any information connected to that

import Vuex, { Store, MutationPayload } from 'vuex'

import eventBus from '../eventbus'

export default class Server {
    uid: string;
    socket?: WebSocket;
    store: Store<any>;

    errorhandler?: () => void;

    constructor (store: Store<any>) {
      console.log('Creating a new server')
      let server = this
      this.uid = ''
      this.loadUid()
      this.store = store
      this.store.subscribe((mutation: MutationPayload, state: any) => {
        server.onmutation(mutation, state)
      })
      eventBus.$on('/chat/send', (data:string) => { server.sendChat(data) })
    }

    connect () {
      console.log('Connecting to the ws server.')
      let port = ':8081'
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

    onmessage (msg:MessageEvent) {
      let packet = JSON.parse(msg.data)
      let type = packet['type']
      let data = packet['data']
      console.log('Received a message: ', packet)
      if (type === 'Session') {
        this.store.commit('setUsername', data['name'])
      } else if (type === 'Chat') {
        this.onchat(data)
      }
    }

    onchat (data : any) {
      eventBus.$emit('/chat/message', { sender: data['sender'], message: data['message'] })
    }

    sendChat (text : string) {
      let packet = {
        type: 'Chat',
        uid: this.uid,
        data: {
          sender: this.store.state.username,
          message: text
        }
      }
      this.send(JSON.stringify(packet))
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
        data: { uid: this.uid }
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

    loadUid () {
      if (localStorage.uid) {
        this.uid = localStorage.uid
      }
      this.uid = ''
      for (var i = 0; i < 32; i++) {
        // pick a char from [33;126]
        let v = 33 + Math.round(Math.random() * 93.5)
        this.uid += String.fromCharCode(v)
      }
      localStorage.uid = this.uid
    }

    onmutation (mutation: MutationPayload, state: any) {
      if (mutation.type === 'setUsername') {
        if (state.username.length > 0) {
          let packet = {
            type: 'SetUsername',
            uid: this.uid,
            data: { name: state.username }
          }
          this.send(JSON.stringify(packet))
        }
      }
    }
}
