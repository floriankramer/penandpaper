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
<div class="quickentry-popup">
  <div class="quickentry-row">
    Id: <input ref="idInput" v-model="id"  pattern="[a-zA-Z0-9_-]*"/>
  </div>
  <div class="quickentry-row">
    Name: <input v-model="name"/>
  </div>
  <div class="quickentry-row">
    Description: <input v-model="description"/>
  </div>
  <div class="quickentry-row">
    Parent: <Autocomplete v-bind:completions="completions" v-model="parent" v-on:autocomplete="onCompleteParent"/>
  </div>
  <div class="quickentry-row">
    <button v-on:click="onClose">Cancel</button><button v-on:click="onCreate">Create</button>
  </div>
</div>
</template>

<script lang="ts">
import { Component, Prop, Vue, Watch } from 'vue-property-decorator'
import Autocomplete, { Completion } from './Autocomplete.vue'
import { Attribute } from './WikiTypes'
import $ from 'jquery'

@Component({ components: {
  Autocomplete
}
})
export default class QuickEntryCreator extends Vue {
  id: string = ''
  name: string = ''
  description: string = ''
  parent: string = ''

  @Prop()
  requestFocus: boolean = false

  completions: Completion[] = []

  onCompleteParent (val: string) {
    $.post('/wiki/quicksearch', val, (resp: any) => {
      let nc : Completion[] = []
      let raw = JSON.parse(resp)
      raw.forEach((rqs: any) => {
        let rq = new Completion()
        rq.value = rqs.id
        rq.text = rqs.name
        nc.push(rq)
      })
      this.completions = nc
    }).fail(() => {
      let r = new Completion()
      r.value = ''
      r.text = 'Quicksearch Failed.'
      this.completions = [r]
    })
  }

  onClose () {
    this.$emit('close', false)
    this.clear()
  }

  onCreate () {
    let attr: Attribute[] = []
    attr.push(new Attribute('text', '', false, false, false))
    attr.push(new Attribute('name', this.name, false, false, false))
    attr.push(new Attribute('description', this.description, true, false, false))
    attr.push(new Attribute('parent', this.parent, false, false, false))
    $.post('/wiki/save/' + this.id, JSON.stringify(attr), (body) => {
      this.$emit('close', true)
      this.clear()
    }).fail(() => {
      alert('Saving failed.')
      this.$emit('close', true)
      this.clear()
    })
  }

  clear () {
    this.id = ''
    this.name = ''
    this.description = ''
    this.parent = ''
  }

  @Watch('requestFocus')
  onRequestFocus (n: boolean, o: boolean) {
    if (n) {
      if (this.$refs.idInput instanceof HTMLElement) {
        var e = this.$refs.idInput as HTMLElement
        e.focus()
      }
    }
  }

  @Watch('id')
  onIdChanged (value: string, old: string) {
    let r = /[^a-zA-Z0-9_-]/
    if (r.test(value)) {
      this.id = value.replace(/[^a-zA-Z0-9_-]/, '_')
    }
  }
}
</script>

<style scoped>
div.quickentry-popup {
  position: absolute;
  border-radius: 12px;
  z-index: 100;
  left: 20px;
  right: 20px;
  top: 20px;
  bottom: 20px;
  padding: 7px;
  overflow: auto;
}

div.quickentry-row {
  padding-bottom: 12px;
}

body.light-mode div.quickentry-popup {
  background-color: #434343;
}

body.dark-mode div.quickentry-popup {
  background-color: #333333;
}
</style>
