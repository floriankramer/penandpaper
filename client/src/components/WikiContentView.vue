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
  <h5>{{entryName}}</h5>
  <details>
    <summary>Attributes</summary>
    <table>
      <tr v-for="attr in displayedAttributes" v-bind:key="attr.idx">
        <td v-bind:class="{ 'interesting-attr': attr.data.isInteresting, 'inheritable-attr': attr.data.isInheritable }">{{attr.data.predicate}}</td><td v-html="attr.data.value"></td>
      </tr>
    </table>
    <p>Inherited:</p>
    <table>
      <tr v-for="attr in inheritedAttributes" v-bind:key="attr.idx">
        <td v-bind:class="{ 'interesting-attr': attr.data.isInteresting, 'inheritable-attr': attr.data.isInheritable }">{{attr.data.predicate}}</td><td v-html="attr.data.value"></td>
      </tr>
    </table>
  </details>
  <hr/>
  <span v-html="content"></span>
</div>
</template>

<script lang="ts">
import { Component, Prop, Vue, Watch } from 'vue-property-decorator'

import { Attribute, DisplayedAttribute } from './WikiTypes'

import $ from 'jquery'

@Component
export default class WikiContentView extends Vue {
  entryName: string = ''
  content: string = ''
  displayedAttributes: DisplayedAttribute[] = []
  inheritedAttributes: DisplayedAttribute[] = []

  @Prop()
  id : string = ''

  @Watch('id')
  onIdChange (value: string, old: string) {
    if (value === '' || value === 'root') {
      this.entryName = ''
      this.content = ''
      this.displayedAttributes = []
      this.inheritedAttributes = []
      return
    }
    $.get('/wiki/get/' + value, (body) => {
      let nDAttr: DisplayedAttribute[] = []
      this.entryName = body.name
      body.direct.forEach((attr: Attribute) => {
        if (attr.predicate === 'text') {
          this.content = attr.value
        } else if (attr.predicate !== 'parent') {
          nDAttr.push(new DisplayedAttribute(nDAttr.length, attr))
        }
      })
      this.displayedAttributes = nDAttr
      nDAttr = []
      body.inherited.forEach((attr: Attribute) => {
        if (attr.predicate === 'text') {
          this.content = attr.value
        } else if (attr.predicate !== 'parent') {
          nDAttr.push(new DisplayedAttribute(nDAttr.length, attr))
        }
      })
      this.inheritedAttributes = nDAttr
    }).fail(() => {
      this.content = 'Unable to load the specified page'
    })
  }

  reload () {
    this.onIdChange(this.id, this.id)
  }
}
</script>

<style scoped>
</style>
