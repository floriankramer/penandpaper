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
  <div v-on:click="onClick">
    <div class="list-view-container" v-for="entry in list" v-bind:key="entry.id" v-html="entry.html"></div>
  </div>
</div>
</template>

<script lang="ts">
import { Component, Prop, Vue, Watch } from 'vue-property-decorator'

export interface ListItem {
  id: string
  html: string
}

@Component
export default class ListView extends Vue {
  @Prop()
  list: ListItem[] = []

  onClick (event: MouseEvent) {
    let target = event.target || event.srcElement

    if (target !== null) {
      var el = target as HTMLElement
      console.log(el.dataset)
      if (el.dataset.event !== undefined) {
        if (el.dataset.payload !== undefined) {
          console.log('emitting', el.dataset.event, el.dataset.payload)
          this.$emit(el.dataset.event, el.dataset.payload)
        } else {
          console.log('emitting', el.dataset.event)
          this.$emit(el.dataset.event)
        }
      }
    }
  }
}
</script>

<style scoped>
div.list-view-item {
  margin-bottom: 7px;
  padding: 4px;
  background-color: #2e2e2e;
  border-radius: 7px;
  display: inline-block;
}
</style>
