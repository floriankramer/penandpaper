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
<input type="text" v-model="value" v-on:keyup.enter="acceptCompletion" v-on:keyup.down="onDown" v-on:keyup.up="onUp" />
<div class="autocomplete-floating">
  <ul v-show="showCompletions" v-on:click.prevent="onClick" class="autocomplete-list">
    <li v-for="(sg, index) in completions" v-bind:key="sg" v-bind:data-value="sg.value"><div v-bind:class="{selected: index == selected}">{{sg.text}}</div></li>
  </ul>
</div>
</div>
</template>

<script lang="ts">
import { Component, Prop, Vue, Watch } from 'vue-property-decorator'

export class Completion {
  text: string = ''
  value: string = ''
}

@Component
export default class Autocomplete extends Vue {
  value: string = ''
  target: string | null = null

  selected: number = -1

  @Prop()
  completions: Completion[] = []

  showCompletions: boolean = false

  @Watch('value')
  onValueChanged (v: string, old: string) {
    if (this.target === null || v !== this.target) {
      this.$emit('autocomplete', v)
      this.showCompletions = true
    }
    this.$emit('input', v)
  }

  acceptCompletion () {
    var s = Math.max(0, this.selected)
    if (this.completions.length > s) {
      this.target = this.completions[s].value
      this.value = this.completions[s].value
      this.showCompletions = false
      this.selected = -1
    }
  }

  onDown () {
    this.selected = Math.min(this.completions.length - 1, this.selected + 1)
  }

  onUp () {
    this.selected = Math.max(-1, this.selected - 1)
  }

  onClick (event: MouseEvent) {
    let target = event.target || event.srcElement

    if (target !== null) {
      var el = target as HTMLElement
      if (el.tagName === 'LI') {
        if (el.dataset.value !== undefined) {
          this.target = el.dataset.value
          this.value = el.dataset.value
          this.showCompletions = false
          this.selected = -1
        }
      } else if (el.tagName === 'DIV') {
        var pel = el.parentElement
        if (pel !== null && pel.dataset.value !== undefined) {
          this.target = pel.dataset.value
          this.value = pel.dataset.value
          this.showCompletions = false
          this.selected = -1
        }
      }
    }
  }
}
</script>

<style scoped>
div.autocomplete-floating {
  position: absolute;
  background-color: #101010;
  border-radius: 7px;
  padding-left: 4px;
  padding-right: 20px;
  max-height: 130px;
  overflow-y: auto;
  overflow-x: hidden;
}

ul.autocomplete-list {
  list-style: none;
  padding-left: 7px;
  width: max-content;
}

ul.autocomplete-list li {
  width: max-content;
}

ul.autocomplete-list li div {
  width: max-content;
  margin-bottom: 7px;
  padding: 4px;
  border-radius: 7px;
  display: inline-block;
  cursor: pointer;
}

body.dark-mode ul.autocomplete-list li div  {
  background-color: #2e2e2e;
}

body.light-mode ul.autocomplete-list li div  {
  background-color: #3e3e3e;
}

ul.autocomplete-list li div:hover {
  background-color: #2b828d !important;
}

ul.autocomplete-list li div.selected {
  background-color: #2b828d !important;
}

</style>
