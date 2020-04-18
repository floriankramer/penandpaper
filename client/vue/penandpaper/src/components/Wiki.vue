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
  <div class="wiki-container">
    <div class="wiki-sidebar">
      <div class="wiki-controls">
        <button v-on:click="newPage">new</button>
        <button v-on:click="editPage">edit</button>
        <button v-on:click="deletePage">delete</button>
        <button v-on:click="loadPage('home')">home</button>
      </div>
      <div class="wiki-index">
        <h3>Pages</h3>
        <p v-for="t in titles" v-bind:key="t" v-on:click="loadPage(t)">
          {{t}}
        </p>
      </div>
    </div>
    <div class="wiki-center">
      <div id="wiki-content" v-on:click.prevent="interceptLink" v-show="showContent" v-html="content">
      </div>
      <div id="wiki-edit" v-show="showEdit">
        <form>
          <div class="with-tooltip">
            <label for="title">id: </label>
            <input name="title" v-model="title"/>
            <span class="tooltip">The unique identifier of this article.</span>
          </div>
          <div>
            <textarea id="wiki-structured" name="structured" v-model="structured"/>
          </div>
          <div>
            <textarea id="wiki-raw-content" name="content" rows="36" cols="80"/>
            <div class="wiki-content-complete" v-bind:style="contentCompleteStyle" v-show="showContentComplete"/>
            <select id="wiki-theme" v-model="cmTheme">
              <option disabled value="">Please select one</option>
              <option value="railscasts">railscasts</option>
              <option value="pastel-on-dark">pastel-on-dark</option>
              <option value="panda-syntax">panda-syntax</option>
              <option value="mbo">mbo</option>
              <option value="lesser-dark">lesser-dark</option>
              <option value="darcula">darcula</option>
            </select>
          </div>
          <div>
            <input type="submit" value="save" v-on:click.prevent="savePage"/>
          </div>
        </form>
      </div>
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue, Watch } from 'vue-property-decorator'
import Server from './server'
import $ from 'jquery'
import getCaretCoordinates from 'textarea-caret'

import CodeMirror, { showHint } from 'codemirror'
import 'codemirror/addon/hint/show-hint.js'
import 'codemirror/addon/hint/show-hint.css'

import 'codemirror/lib/codemirror.css'

import 'codemirror/theme/railscasts.css'
import 'codemirror/theme/pastel-on-dark.css'
import 'codemirror/theme/panda-syntax.css'
import 'codemirror/theme/mbo.css'
import 'codemirror/theme/lesser-dark.css'
import 'codemirror/theme/darcula.css'

@Component
export default class Wiki extends Vue {
  showContent: boolean = false
  showEdit: boolean = true

  titles: string[] = []

  title: string = 'home'
  content: string = 'Lorem Ipsum...'
  rawContent: string = ''
  rawContentElem: HTMLTextAreaElement | null = null
  showContentComplete: boolean = false
  contentCompleteStyle: any = { left: '0px', top: '0px' }
  cmTheme: string = 'panda-syntax'

  cmEditor: CodeMirror.EditorFromTextArea | null = null

  structured: string = ''
  structuredElem: HTMLTextAreaElement | null = null

  newPage () {
    this.showContent = false
    this.showEdit = true
    this.title = 'id'
    this.rawContent = ''
  }

  editPage () {
    this.showContent = false
    this.showEdit = true
    $.get('/wiki/raw/' + this.title, (body) => {
      this.rawContent = body
      if (this.cmEditor !== null) {
        this.cmEditor.setValue(this.rawContent)
      }
    }).fail(() => {
      this.rawContent = 'Unable to load the specified page'
    })
  }

  deletePage () {
    $.get('/wiki/delete/' + this.title, () => {
      this.loadIndex()
    }).fail(() => {
      alert('Deleting the article failed.')
    })
  }

  savePage () {
    $.post('/wiki/save/' + this.title, this.rawContent, (body) => {
      this.loadIndex()
      this.loadPage(this.title)
    }).fail(() => {
      alert('Saving failed.')
    })
  }

  loadPage (id: string) {
    this.showContent = true
    this.showEdit = false
    this.title = id
    $.get('/wiki/get/' + id, (body) => {
      this.content = body
    }).fail(() => {
      this.content = 'Unable to load the specified page'
    })
  }

  interceptLink (event: Event) {
    let target = event.target || event.srcElement

    if (target !== null) {
      var el = target as HTMLElement
      if (el.tagName === 'A') {
        // intercept the link
        let target = el.getAttribute('href')
        if (target !== null) {
          this.loadPage(target)
        }
      }
    }
  }

  async autocompleteContent (cm: CodeMirror.Editor, callback: (hints: CodeMirror.Hints) => any) {
    const MAX_CTX_SIZE = 40
    const CTX_TARGET_SIZE = 24
    let c = cm.getCursor()
    // Ignore the new space
    c = new CodeMirror.Pos(c.line, c.ch)
    // The extend of the context
    let start = new CodeMirror.Pos(c.line, c.ch)
    let end = new CodeMirror.Pos(c.line, c.ch)
    // Extract at least 16 and up to 32 characters from the text.
    let ctx = ''
    for (let i = 0; i < MAX_CTX_SIZE && !(c.line < 0 && c.ch < 0); ++i) {
      let line = cm.getLine(c.line)
      let char = line[c.ch]
      if (char !== undefined) {
        // We read at least 16 characters and are on a word boundary
        if (i > CTX_TARGET_SIZE && (char === ' ' || char === '\r' || char === '\n')) {
          break
        }
        // Update the start marker of the ctx and add the character
        start.line = c.line
        start.ch = c.ch
        ctx = char + ctx
      }
      c.ch -= 1
      // skip lines upwards until we reach a nonempty line or the start
      while (c.ch < 0) {
        c.line -= 1
        if (c.line <= 0) {
          // We reached the beginning of the document
          c.line = -1
          c.ch = -1
          break
        }
        c.ch = cm.getLine(c.line).length - 1
      }
    }
    let hints = { list: [], from: start, to: end } as CodeMirror.Hints
    return new Promise((resolve, reject) => {
      $.post('/wiki/complete', JSON.stringify({ context: ctx }), (result) => {
        result.forEach((hint: any) => {
          hints.list.push({
            text: hint.value,
            displayText: hint.name + ' (' + hint.replaces + ')'
          })
        })
        resolve(hints)
      }).fail(() => {
        reject(new Error('Unable to contact the server'))
      })
    })
  }

  mounted () {
    this.loadIndex()
    if (this.rawContentElem === null) {
      this.rawContentElem = document.getElementById('wiki-raw-content') as (HTMLTextAreaElement | null)
    }
    if (this.rawContentElem !== null) {
      this.cmEditor = CodeMirror.fromTextArea(this.rawContentElem, {
        lineNumbers: true,
        theme: 'panda-syntax',
        extraKeys: {
          'Space': (cm: CodeMirror.Editor) => {
            // TODO: This continues calling the autocompleteContent function
            // even after other characters where pressed
            cm.showHint({
              hint: this.autocompleteContent,
              completeSingle: false,
              closeCharacters: new RegExp('\\S')
            })
            cm.replaceSelection(' ')
          }
        }
      })
      this.cmEditor.on('changes', this.onTextChanged)
    } else {
      console.log('Warning: Unable to initialize code mirror, as the required text area was not found')
    }
  }

  onTextChanged () {
    if (this.cmEditor !== null) {
      this.rawContent = this.cmEditor.getValue()
    }
  }

  relayoutStructured (allowTrailing: boolean = false) {
    let newVal = ''
    let val = this.structured
    if (val.length > 0) {
      let isWs = val[0] === ' ' || val[0] === '\r' || val[0] === '\n'
      let hadWs = false
      for (let i = 0; i < val.length; ++i) {
        let c = val[i]
        if (c === ' ' || c === '\r' || c === '\n') {
          isWs = true
        } else {
          if (isWs) {
            isWs = false
            if (hadWs) {
              newVal += '\n'
              hadWs = false
            } else {
              newVal += '    '
              hadWs = true
            }
          }
          newVal += c
        }
      }
      if (allowTrailing && isWs) {
        if (hadWs) {
          newVal += '\n'
          hadWs = false
        } else {
          newVal += '    '
          hadWs = true
        }
      }
    }
    this.structured = newVal
  }

  @Watch('cmTheme')
  onThemeChange (value: string, old: string) {
    if (this.cmEditor) {
      this.cmEditor.setOption('theme', value)
    }
  }

  @Watch('structured')
  onStructuredChanged (value: string, old: string) {
    if (this.structuredElem === null) {
      this.structuredElem = document.getElementById('wiki-structured') as (HTMLTextAreaElement | null)
    }
    if (this.structuredElem !== null) {
      let pos = this.structuredElem.selectionStart
      let input = ''
      if (pos >= 0 && pos - 1 < value.length) {
        input = value[pos - 1]
      }
      if (value.length < old.length) {
        this.relayoutStructured()
      } else if (input === ' ' || input === '\t') {
        this.relayoutStructured(true)
      }
    } else {
      console.log('Unable to find the wiki-structured element')
    }
    // TODO: Autocomplete (switch over to code mirror)
  }

  loadIndex () {
    $.get('/wiki/list', (body) => {
      let titles = JSON.parse(body)
      if (Array.isArray(titles)) {
        this.titles = titles
      }
    })
  }
}
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
div .wiki-sidebar {
  position: absolute;
  width: 100px;
  top: 0px;
  bottom: 0px;
  left: 0px;
  padding-left: 5px;
  padding-right: 5px;
  border-right: 3px solid black;
}

div .wiki-sidebar button {
  margin-top: 17px;
  width: 90px;
}

div .wiki-center {
  overflow-y: auto;
  position: absolute;
  left: 110px;
  top: 0px;
  bottom: 0px;
  right: 0px;
  padding: 7px;
}

#wiki-edit textarea {
  width: 100%;
  height: 100%;
  color: white;
  background-color: rgb(61, 61, 61);
  border: 3px solid black;
}

/*Tooltip from https://www.w3schools.com/css/css_tooltip.asp */

/* Tooltip text */
.with-tooltip .tooltip {
  visibility: hidden;
  width: 220px;
  background-color: black;
  color: #fff;
  text-align: center;
  padding: 5px 0;
  border-radius: 6px;

  /* Position the tooltip text - see examples below! */
  position: absolute;
  z-index: 1;
}

/* Show the tooltip text when you mouse over the tooltip container */
.with-tooltip:hover .tooltip {
  visibility: visible;
}

div .wiki-content-complete {
  width: 200px;
  height: 100px;
  position: absolute;
  background-color: #333333;
  border-radius: 5px;
  border: 1px solid black;
}

</style>

<style>
.CodeMirror {
  font-family: Avenir,Helvetica,Arial,sans-serif !important;
}
</style>
