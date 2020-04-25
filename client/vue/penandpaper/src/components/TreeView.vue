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
<div v-html="html" v-on:click="onClick">
</div>
</div>
</template>

<script lang="ts">
import { Component, Prop, Vue, Watch } from 'vue-property-decorator'

export interface TreeItem {
  html: string
  children: TreeItem[]
}

class DfsLevel {
  item: TreeItem
  childIndex: number = 0
  html: string = ''

  constructor (item: TreeItem) {
    this.item = item
  }
}

@Component
export default class Wiki extends Vue {
  html: string = ''

  @Prop()
  tree: TreeItem | null = null

  onClick (event: MouseEvent) {
    let target = event.target || event.srcElement

    if (target !== null) {
      var el = target as HTMLElement
      if (el.tagName === 'LI') {
        if (el.classList.contains('tree-view-hidden')) {
          el.classList.remove('tree-view-hidden')
          el.classList.add('tree-view-visible')
        } else {
          el.classList.add('tree-view-hidden')
          el.classList.remove('tree-view-visible')
        }
      } else {
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

  @Watch('tree')
  onRebuildTree () {
    console.log('Rebuilding the tree view')
    if (this.tree === null) {
      this.html = ''
      return
    }
    // Dfs on the tree
    let rootLevel = new DfsLevel(this.tree)
    rootLevel.html = '<ul class="tree-view"><li class="tree-view-visible tree-view">' + this.tree.html
    let stack : DfsLevel[] = [rootLevel]
    while (stack.length > 0) {
      let l = stack[stack.length - 1]
      if (l.childIndex >= l.item.children.length) {
        // we are done with this node
        if (stack.length === 1) {
          if (l.item.children.length > 0) {
            l.html += '</ul>'
          }
          l.html += '</li></ul>'
          // we are done with the root node
          break
        }
        let parent = stack[stack.length - 2]
        // todo add the list
        // parent.j["children"].push_back(l.j)
        if (parent.childIndex === 1) {
          parent.html += '<ul class="tree-view">'
        }
        if (l.item.children.length > 0) {
          l.html += '</ul>'
        }
        l.html += '</li>'
        parent.html += l.html
        stack.pop()
      } else {
        // the node has more children, add the next child to the stack
        let ci = l.childIndex
        let child = l.item.children[ci]
        l.childIndex++
        let html = '<li class="tree-view tree-view-visible">' + child.html
        let cl = new DfsLevel(child)
        cl.html = html
        stack.push(cl)
      }
    }
    if (stack.length > 0) {
      this.html = stack[0].html
    } else {
      console.log('Error while building a tree view: the stack is empty.')
    }
  }
}
</script>

<style>
ul.tree-view {
  list-style: none;
  padding: 7px;
  width: max-content;
}

li.tree-view {
  width: max-content;
}

.tree-view-hidden ul {
  display: none;
}

.tree-view-hidden::before {
  content: "+ ";
}

.tree-view-visible::before {
  content: "- ";
}
</style>
