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
    <input class="chat-input" v-model="currentText" v-on:keyup.enter="send"/>
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

  constructor () {
    super()
    eventBus.$on('/chat/message', this.onMessage)
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

  send () {
    eventBus.$emit('/chat/send', this.currentText)
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
