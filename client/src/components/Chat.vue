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
  <div class="chat-area">
    <!-- We need a second container here for the docking library, as paddings break otherwise. -->
    <div class="chat-flex-container">
      <div id="chat-text" class="chat-text">
        <dl>
          <template v-for="chat in chatHistory">
            <dt v-bind:key="'sender' + chat.id">{{chat.from}}</dt>
            <dd v-bind:key="'msg' + chat.id" v-html="chat.text"></dd>
          </template>
        </dl>
      </div>
      <input class="chat-input" v-model="currentText" v-on:keyup.up="up" v-on:keyup.down="down" v-on:keyup.enter="send"/>
    </div>
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

  chatArea: HTMLDivElement | undefined = undefined

  constructor () {
    super()
    eventBus.$on('/chat/message', this.onMessage)

    let greeting = new Message()
    greeting.id = -1
    greeting.from = 'Server'
    greeting.text = 'Welcome to GOATS ROCK!'
    this.chatHistory.push(greeting)
  }

  mounted () {
    this.chatArea = this.$el.getElementsByClassName('chat-text').item(0) as HTMLDivElement
  }

  escapeText (text: string) : string {
    return text.replace(/<br\/>/g, '\n')
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&#039;')
      .replace(/\n/g, '<br/>')
  }

  onMessage (data : any) {
    let scroll = false
    if (this.chatArea !== undefined) {
      if (this.chatArea.scrollTop + this.chatArea.clientHeight >= this.chatArea.scrollHeight - 10) {
        scroll = true
      }
    }
    let m = new Message()
    m.from = data['sender']
    m.text = this.escapeText(data['message'])
    m.id = this.nextId
    this.nextId++
    this.chatHistory.push(m)
    // Ensure we never store more than the last 100 messages
    if (this.chatHistory.length > 100) {
      this.chatHistory.splice(0, 100 - this.chatHistory.length)
    }
    if (scroll) {
      // TODO: this is kinda of an ugly hack that tries to handle a delayed dom update
      setTimeout(() => {
        if (this.chatArea !== undefined) {
          console.log('scrolling')
          this.chatArea.scrollTo({
            top: this.chatArea.scrollHeight,
            left: 0,
            behavior: 'smooth'
          })
        }
      }, 50)
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

div .chat-flex-container {
  display: flex;
  flex-direction: column;
  height: calc(100% - 20px);
  padding: 10px;
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
