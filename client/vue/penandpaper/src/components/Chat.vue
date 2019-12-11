<template>
  <div class="chat-area">
    <div class="chat-text">
      <dl>
        <template v-for="chat in chatHistory">
          <dt v-bind:key="'sender' + chat.id">{{chat.from}}</dt>
          <dd v-bind:key="'msg' + chat.id">{{chat.text}}</dd>
        </template>
      </dl>
    </div>
    <input class="chat-input" v-model="currentText" v-on:keyup.up="up" v-on:keyup.down="down" v-on:keyup.enter="send"/>
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator'
import Server from './server'
import eventBus from '../eventbus'

class Message {
  id: number = 0;
  from: string = '';
  text: string = '';
}

@Component
export default class Chat extends Vue {
  chatHistory: Message[] = []
  nextId: number = 0
  currentText: string = ''

  historyPos: number = 0
  sentHistory: string[] = []

  constructor () {
    super()
    eventBus.$on('/chat/message', this.onMessage)

    let greeting = new Message()
    greeting.id = -1
    greeting.from = 'Server'
    greeting.text = 'Welcome to GOATS ROCK!'
    this.chatHistory.push(greeting)
  }

  onMessage (data : any) {
    let m = new Message()
    m.from = data['sender']
    m.text = data['message']
    this.chatHistory.push(m)
    // Ensure we never store more than the last 100 messages
    if (this.chatHistory.length > 100) {
      this.chatHistory.splice(0, 100 - this.chatHistory.length)
    }
  }

  up () {
    if (this.sentHistory.length === 0) {
      return
    }
    if (this.historyPos === 0) {
      this.sentHistory.push(this.currentText)
    }
    this.historyPos++
    this.historyPos = Math.min(this.historyPos, this.sentHistory.length - 1)
    this.currentText = this.sentHistory[this.sentHistory.length - this.historyPos - 1]
  }

  down () {
    if (this.sentHistory.length === 0 || this.historyPos === 0) {
      return
    }
    this.historyPos--
    this.currentText = this.sentHistory[this.sentHistory.length - this.historyPos - 1]
    if (this.historyPos === 0) {
      this.sentHistory.pop()
    }
  }

  send () {
    eventBus.$emit('/chat/send', this.currentText)

    if (this.historyPos !== 0) {
      this.historyPos = 0
      this.sentHistory.pop()
    }
    this.sentHistory.push(this.currentText)
    if (this.sentHistory.length > 100) {
      this.sentHistory.splice(0, this.sentHistory.length - 100)
    }
    this.currentText = ''
  }
}
</script>

<style scoped>
div .chat-area {
  display: flex;
  flex-direction: column;
  height: 100%;
  padding-bottom: 5px;
  padding-top: 5px;
}

div .chat-text {
  flex-grow: 1;
  overflow-y: auto;
  text-align: left;
}

div .chat-input {
  margin-bottom: 10px;
}
</style>
