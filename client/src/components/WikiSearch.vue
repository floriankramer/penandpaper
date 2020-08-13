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
  <div>
    <input type="text" v-model="searchTerm" v-on:keydown.enter="search">
  </div>
  <Attributes v-model="attributes"/>
  <button v-on:click="search">search</button>
  <hr/>
  <ListView :list="results" v-on:show="onShow" />
</div>
</template>

<script lang="ts">
import { Component, Prop, Vue, Watch } from 'vue-property-decorator'
import $ from 'jquery'

import Attributes from './Attributes.vue'
import ListView, { ListItem } from './ListView.vue'

import { Attribute } from './WikiTypes'
import eventbus from '../eventbus'

class SearchResult implements ListItem {
  id: string = ''
  html: string = ''
}

@Component({
  components: {
    Attributes,
    ListView
  }
})
export default class WikiSearch extends Vue {
  searchTerm: string = ''

  attributes: Attribute[] = []
  results: SearchResult[] = []

  mounted () {
    this.attributes = [new Attribute('', '', false, false, false)]
  }

  onShow (id: string) {
    this.$emit('show', id)
  }

  search () {
    let query = {
      'search-term': this.searchTerm,
      'filters': [] as any[]
    }
    this.attributes.forEach((a: Attribute) => {
      if (a.predicate.length > 0) {
        query.filters.push({
          predicate: a.predicate,
          value: a.value
        })
      }
    })
    $.post('/wiki/search', JSON.stringify(query), (res) => {
      let newResults: SearchResult[] = []
      res.forEach((r: any) => {
        let l = new SearchResult()
        l.id = r.id
        l.html = '<span class="result" data-event="show" data-payload="' + r.id + '">' + r.name + '</span>'
        newResults.push(l)
      })
      this.results = newResults
    }).fail(() => {
      eventbus.$emit('/notification', 'The search failed.')
    })
  }
}
</script>

<style>
span.result {
  cursor: pointer;
  display: block;
  margin-bottom: 7px;
  padding: 3px;
}
</style>
