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
<input ref="input"
       type="text"
       v-model="value"
       v-on:keyup.enter="acceptCompletion"
       v-on:keyup.down="onDown"
       v-on:keyup.up="onUp"
       v-on:blur="onBlur" />
<div class="autocomplete-floating" v-show="showCompletions">
  <ul v-on:click.prevent="onClick" class="autocomplete-list">
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
  @Prop()
  value: string = ''

  target: string | null = null

  selected: number = -1

  @Prop()
  completions: Completion[] = []

  showCompletions: boolean = false

  focus () {
    var e = this.$refs.input as HTMLElement
    e.focus()
  }

  setValue (v: string) {
    this.value = v
  }

  @Watch('value')
  onValueChanged (v: string, old: string) {
    if ((this.target === null || v !== this.target) && v.length > 0) {
      this.$emit('autocomplete', v)
    } else if (v.length === 0) {
      this.showCompletions = false
    }
    this.$emit('input', v)
  }

  @Watch('completions')
  onCompletionsChanged (v: Completion[]) {
    var input = this.$refs.input as HTMLInputElement
    this.showCompletions = v !== undefined && v.length > 0 && input === document.activeElement
  }

  setCompletions (completions: Completion[]) {
    this.completions = completions
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

  onBlur () {
    this.showCompletions = false
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
  display: block;
  background-color: #101010;
  border-radius: 7px;
  padding-left: 4px;
  padding-right: 20px;
  max-height: 130px;
  overflow-y: auto;
  overflow-x: hidden;
  z-index: 999;
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
