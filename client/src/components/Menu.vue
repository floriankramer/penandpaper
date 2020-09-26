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
  <div class="menu-container">
    <ul class="menu-root" v-on:click="onClick">
      <li class="menu-root" v-for="menu in menus" v-bind:key="menu.text">
        {{menu.text}}
        <ul class="menu-item">
          <li class="menu-item" v-for="item in menu.items" v-bind:key="item.id" v-bind:data-id="item.id">
            {{item.text}}
          </li>
        </ul>
      </li>
    </ul>
  </div>
</div>
</template>

<script lang="ts">
import { Component, Prop, Vue, Watch } from 'vue-property-decorator'

export interface MenuGroup {
  text: string
  items: MenuItem[]
}

export interface MenuKeyShortcut {
  key: string
  alt?: boolean
  ctrl?: boolean
  shift?: boolean
}

export interface MenuItem {
  id: string
  text: string
  shortcut?: MenuKeyShortcut
}

class KeyAction {
  shortcut: MenuKeyShortcut
  id: string

  constructor (shortcut: MenuKeyShortcut, id: string) {
    this.shortcut = shortcut
    this.id = id
  }
}

@Component
export default class Menu extends Vue {
  html: string = ''

  @Prop()
  menus: MenuGroup[] = []

  keyActions: Map<string, KeyAction> = new Map()

  mounted () {
    document.addEventListener('keydown', this.onKeyDown)
  }

  onClick (event: MouseEvent) {
    let target = event.target || event.srcElement

    if (target !== null) {
      var el = target as HTMLElement
      if (el.tagName === 'LI') {
        let id = el.dataset['id']
        if (id !== undefined) {
          this.$emit('selected', id)
        }
      }
    }
  }

  @Watch('menus')
  onMenusChanged (v: MenuGroup[], old: MenuGroup[]) {
    this.keyActions = new Map()
    v.forEach((g: MenuGroup) => {
      g.items.forEach((i: MenuItem) => {
        if (i.shortcut !== undefined) {
          this.keyActions.set(i.shortcut.key, new KeyAction(i.shortcut, i.id))
        }
      })
    })
  }

  onKeyDown (event: KeyboardEvent) {
    let a = this.keyActions.get(event.key)
    if (a !== undefined) {
      if ((a.shortcut.alt !== true || event.altKey) &&
          (a.shortcut.ctrl !== true || event.ctrlKey) &&
          (a.shortcut.shift !== true || event.shiftKey)) {
        this.$emit('selected', a.id)
        event.preventDefault()
      }
    }
  }
}
</script>
<style scoped>
div.menu-container {
  width: 100%;
  height: 100%;
  position: absolute;
  background-color: #111111;
}

ul.menu-root {
  list-style: none;
  margin-top: 2px;
  margin-bottom: 0px;
}

li.menu-root {
  float: left;
  margin-right: 20px;
}

ul.menu-item {
  list-style: none;
  display: none;
  position: absolute;
  z-index: 2002;
  background-color: #111111;
  padding: 5px;
  border-radius: 3px;
}

li.menu-item {
  padding: 3px;
  margin-bottom: 3px;
  border-radius: 3px;
}

li.menu-item:hover {
  background-color: #2b828d !important;
}

li.menu-root:hover ul.menu-item {
  display: block !important;
}
</style>
