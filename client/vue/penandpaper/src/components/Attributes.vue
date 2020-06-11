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
<div>
  <table>
    <tr>
      <th>int inh date</th>
      <th>Predicate</th>
      <th>Value</th>
    </tr>
    <tr v-for="(attr, index) in value" v-bind:key="attr">
      <td>
        <input type="checkbox" title="Interesing" v-model="attr.isInteresting" v-on:value="valueChanged">
        <input type="checkbox" title="Inheritable" v-model="attr.isInheritable" v-on:value="valueChanged">
        <input type="checkbox" title="Date" v-model="attr.isDate" v-on:value="valueChanged">
      </td>
      <td>
        <Autocomplete :ref="'predicate' + index"
         v-model="attr.predicate"
         v-on:value="valueChanged"
         v-on:keydown.backspace="(e) => {deleteAttr(e, index)}"
         v-on:keydown.enter="moveToValue(index)"
         v-on:autocomplete="(v) => {onAutocompletePredicate(index, v)}">
      </td>
      <td>
        <input type="text" :ref="'value' + index" v-model="attr.value" v-on:value="valueChanged" v-on:keydown.enter="newAttr(index)" v-on:keydown.backspace="moveToPredicate(index)">
      </td>
    </tr>
  </table>
</div>
</template>

<script lang="ts">
import { Component, Prop, Vue, Watch } from 'vue-property-decorator'
import Autocomplete, { Completion } from './Autocomplete.vue'
import $ from 'jquery'

import { Attribute } from './WikiTypes'

@Component({
  components: {
    Autocomplete
  }
})
export default class Attributes extends Vue {
  @Prop()
  value: Attribute[] = []

  @Watch('value')
  valueChanged () {
    this.$emit('input', this.value)
    // TODO: this is a hack to force an update of the childs values.
    // The hack is used, as he input fields inside of the autocomplete
    // components are otherwise empty when an article is initially edited
    // and only apear once one of the attribute text fields is modified.
    Vue.nextTick(() => {
      for (let i = 0; i < this.value.length; ++i) {
        let e = (this.$refs['predicate' + i] as Autocomplete[])[0]
        e.setValue(this.value[i].predicate)
      }
    })
  }

  onAutocompletePredicate (index: number, value: string) {
    let j = { 'context': value }
    let autocomplete: Autocomplete = (this.$refs['predicate' + index] as Autocomplete[])[0]
    $.post('/wiki/complete/predicate', JSON.stringify(j), (resp: any) => {
      let nc : Completion[] = []
      resp.forEach((rqs: any) => {
        let rq = new Completion()
        rq.value = rqs.value
        rq.text = rqs.name
        nc.push(rq)
      })
      autocomplete.setCompletions(nc)
    }).fail(() => {
      let r = new Completion()
      r.value = ''
      r.text = 'Quicksearch Failed.'
      autocomplete.setCompletions([r])
    })
  }

  deleteAttr (event: KeyboardEvent, index: number) {
    if (this.value[index].predicate.length === 0 && this.value.length > 1) {
      this.value.splice(index, 1)
      // The press of backspace deleted the attribute, we don't want it to delete
      // a character in the newly selected attributes value
      event.preventDefault()
      Vue.nextTick(() => {
        var idx = Math.max(0, index - 1)
        var e = this.$refs['value' + idx] as HTMLElement[]
        e[0].focus()
      })
    }
  }

  newAttr (index: number) {
    this.value.splice(index + 1, 0, new Attribute('', '', false, false, false))
    Vue.nextTick(() => {
      var e = this.$refs['predicate' + (index + 1)] as HTMLElement[]
      e[0].focus()
    })
  }

  moveToValue (index: number) {
    var e = this.$refs['value' + index] as HTMLElement[]
    e[0].focus()
  }

  moveToPredicate (index: number) {
    if (this.value[index].value.length === 0) {
      var e = this.$refs['predicate' + index] as HTMLElement[]
      e[0].focus()
    }
  }
}
</script>

<style>
</style>
