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
  <table>
    <tr>
      <td>Id:</td>
      <td><input ref="idInput" v-model="id"  pattern="[a-zA-Z0-9_-]*"/></td>
    </tr>
    <tr class="quickentry-row">
      <td>Name:</td>
      <td><input v-model="name"/></td>
    </tr>
    <tr class="quickentry-row">
      <td>Description:</td>
      <td><input v-model="description"/></td>
    </tr>
    <tr class="quickentry-row">
      <td>Parent:</td>
      <td><Autocomplete style="display: inline-block;" v-bind:completions="completions" v-model="parent" v-on:autocomplete="onCompleteParent"/></td>
    </tr>
  </table>
  <div class="quickentry-divider"></div>
  <div class="quickentry-row">
    <button v-on:click="onClose">Cancel</button><button v-on:click="onCreateAndEdit">Create and Edit</button><button v-on:click="onCreate">Create</button>
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

  completions: Completion[] = []

  onCompleteParent (val: string) {
    $.post('/wiki/quicksearch', val, (resp: any) => {
      let nc : Completion[] = []
      resp.forEach((rqs: any) => {
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

  onCreateAndEdit () {
    this.save((body) => {
      this.$emit('closeAndEdit', true, this.id)
      this.clear()
    }, () => {
      alert('Saving failed.')
      this.$emit('close', false)
      this.clear()
    })
  }

  onCreate () {
    this.save((body) => {
      this.$emit('close', true)
      this.clear()
    }, () => {
      alert('Saving failed.')
      this.$emit('close', false)
      this.clear()
    })
  }

  save (success: ((res: any) => void), fail: (() => void)) {
    let attr: Attribute[] = []
    attr.push(new Attribute('text', '', false, false, false))
    attr.push(new Attribute('name', this.name, false, false, false))
    attr.push(new Attribute('description', this.description, true, false, false))
    attr.push(new Attribute('parent', this.parent, false, false, false))
    $.post('/wiki/save/' + this.id, JSON.stringify(attr), success).fail(fail)
  }

  clear () {
    this.id = ''
    this.name = ''
    this.description = ''
    this.parent = ''
  }

  @Watch('id')
  onIdChanged (value: string, old: string) {
    let r = /[^a-zA-Z0-9_-]/
    if (r.test(value)) {
      this.id = value.replace(/[^a-zA-Z0-9_-]/, '_')
    }
  }

  setNameAndId (name: string) {
    this.name = name
    this.id = name
  }

  requestFocus () {
    if (this.$refs.idInput instanceof HTMLElement) {
      var e = this.$refs.idInput as HTMLElement
      e.focus()
    }
  }
}
</script>

<style scoped>
div.quickentry-popup {
  position: absolute;
  border-radius: 12px;
  z-index: 100;
  top: 50%;
  left: 50%;
  transform: translate(-50%, -50%);
  padding: 25px;
  overflow: auto;
  box-shadow: black 0px 0px 5px;
}

.quickentry-row {
  padding-bottom: 12px;
}

.quickentry-divider {
  margin-top: 14px;
}

body.light-mode div.quickentry-popup {
  background-color: #434343;
}

body.dark-mode div.quickentry-popup {
  background-color: #333333;
}

button {
  margin-right: 20px;
}
</style>
