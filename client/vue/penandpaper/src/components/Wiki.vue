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
      <span v-on:click="showFind">Find</span><br/>
      <span v-on:click="showTimeline">Timeline</span>
      <hr/>
      <TreeView v-bind:tree="indexTree" v-on:show="loadPage" v-on:new="newPage" v-on:edit="editPage" v-on:delete="deletePage" v-on:autoLink="autoLink" v-on:autoLinkAll="autoLinkAll"/>
    </div>
    <div class="wiki-center">
      <div id="wiki-content" v-on:click="interceptLink" v-show="currentPage == 0" >
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
      <div id="wiki-edit" v-show="currentPage == 1">
        <form>
          <div class="with-tooltip">
            <label for="id">id: </label>
            <input name="id" v-model="id" pattern="[a-zA-Z0-9_-]*"/>
            <span class="tooltip">The unique identifier of this article.</span>
          </div>
          <div>
            <textarea id="wiki-structured" name="structured" v-model="structured" rows="7"/>
            <span style="font-size: 9pt;">
              Attribute modifiers: ! : interesting, + : inheritable, - : date
            </span>
            <details>
              <summary>Inherited Attributes</summary>
              <table>
                <tr v-for="attr in inheritedAttributes" v-bind:key="attr.idx">
                  <td v-bind:class="{ 'interesting-attr': attr.data.isInteresting, 'inheritable-attr': attr.data.isInheritable }">{{attr.data.predicate}}</td><td v-html="attr.data.value"></td>
                </tr>
              </table>
            </details>
          </div>
          <hr/>
          <div>
            <textarea id="wiki-raw-content" name="content" rows="36" cols="80"/>
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
      <div id="wiki-timeline" v-on:click="interceptLink" v-show="currentPage == 2">
        <h2>Timeline</h2>
        <table>
          <tr v-for="event in timeline"
              v-bind:key="event.date + event.predicate + event.id"
              v-bind:class="{'timeline-hr': event.firstDifferentField == 0}">
            <td>{{event.date}}</td>
            <td v-bind:class="{'timeline-color1': event.color == 0, 'timeline-color2': event.color == 1, 'timeline-color3': event.color == 2}">
              {{event.predicate}} <a v-bind:href="event.id">{{event.name}}</a>
            </td>
          </tr>
        </table>
      </div>
      <div id="wiki-find" v-show="currentPage == 3">
        <h2>Find Entries</h2>
      </div>
    </div>
    <div class="wiki-sidebar-right">
      <details v-for="context in contextEntries" v-bind:key="context.idx">
        <summary>{{context.name}}</summary>
         <table>
          <tr v-for="attr in context.data" v-bind:key="attr.idx">
            <td v-bind:class="{ 'interesting-attr': attr.data.isInteresting, 'inheritable-attr': attr.data.isInheritable }">{{attr.data.predicate}}</td><td v-html="attr.data.value"></td>
          </tr>
        </table>
      </details>
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue, Watch } from 'vue-property-decorator'
import Server from './server'
import $ from 'jquery'

import TreeView, { TreeItem } from './TreeView.vue'

import CodeMirror, { showHint } from 'codemirror'
import 'codemirror/addon/hint/show-hint.js'
import 'codemirror/addon/hint/show-hint.css'
import 'codemirror/mode/markdown/markdown.js'

import 'codemirror/lib/codemirror.css'
import 'codemirror/addon/display/rulers.js'

import 'codemirror/theme/railscasts.css'
import 'codemirror/theme/pastel-on-dark.css'
import 'codemirror/theme/panda-syntax.css'
import 'codemirror/theme/mbo.css'
import 'codemirror/theme/lesser-dark.css'
import 'codemirror/theme/darcula.css'
import { DockNode } from 'dock-spawn-ts/lib/js/DockNode'

class IndexTreeItem {
  id: string = ''
  html: string = ''
  children: IndexTreeItem[] = []
}

class Attribute {
  predicate: string = ''
  value: string = ''
  isInteresting: boolean = false
  isInheritable: boolean = false
  isDate: boolean = false

  constructor (predicate: string, value: string, isInteresting: boolean, isInheritable: boolean, isDate: boolean) {
    this.predicate = predicate
    this.value = value
    this.isInteresting = isInteresting
    this.isInheritable = isInheritable
    this.isDate = isDate
  }
}

class DisplayedAttribute {
  idx: number
  data: Attribute
  constructor (idx: number, data: Attribute) {
    this.idx = idx
    this.data = data
  }
}

class ContextEntry {
  idx: number = 0
  name: string = ''
  data: DisplayedAttribute[] = []
}

class TimelineEvent {
  date: string = ''
  name: string = ''
  id: string = ''
  predicate: string = ''
  firstDifferentField: number = 0
  color: number = 0
}

enum CurrentPage {
    VIEW,
    EDIT,
    TIMELINE,
    FIND
}

@Component({
  components: {
    TreeView
  }
})
export default class Wiki extends Vue {
  currentPage: CurrentPage = CurrentPage.VIEW

  entryName: string = ''
  id: string = ''
  content: string = ''
  displayedAttributes: DisplayedAttribute[] = []
  inheritedAttributes: DisplayedAttribute[] = []
  contextEntries: ContextEntry[] = []

  rawContent: string = ''
  rawContentElem: HTMLTextAreaElement | null = null
  cmTheme: string = 'panda-syntax'

  cmEditor: CodeMirror.EditorFromTextArea | null = null

  structured: string = ''
  structuredElem: HTMLTextAreaElement | null = null

  indexTree: IndexTreeItem = new IndexTreeItem()

  timeline: TimelineEvent[] = []

  newPage (parent: string) {
    this.currentPage = CurrentPage.EDIT
    this.id = ''
    this.rawContent = ''
    if (parent.length > 0) {
      this.structured = 'parent    ' + parent
    } else {
      this.structured = 'parent    root'
    }
    this.inheritedAttributes = []
    this.initCodeMirror()
    if (this.cmEditor != null) {
      this.cmEditor.setValue(this.rawContent)
    }
    if (parent.length > 0) {
      this.loadContext(parent)
    } else {
      this.contextEntries = []
    }
  }

  editPage (id: string) {
    this.id = id
    this.currentPage = CurrentPage.EDIT
    $.get('/wiki/raw/' + this.id, (body) => {
      this.initCodeMirror()
      let msg = JSON.parse(body)
      let structured = ''
      let regexWs = new RegExp('\\s')
      msg.direct.forEach((attr: any) => {
        if (attr.predicate === 'text') {
          this.rawContent = attr.value
          if (this.cmEditor !== null) {
            this.cmEditor.setValue(attr.value)
          } else {
            console.log('The cm editor is null on wiki/raw response')
          }
        } else {
          let predWs = regexWs.test(attr.predicate)
          if (predWs) {
            structured += '"'
          }
          if (attr.isInteresting) {
            structured += '!'
          }
          if (attr.isInheritable) {
            structured += '+'
          }
          if (attr.isDate) {
            structured += '-'
          }
          structured += attr.predicate
          if (predWs) {
            structured += '"'
          }
          structured += '    '
          if (regexWs.test(attr.value)) {
            structured += '"' + attr.value + '"'
          } else {
            structured += attr.value
          }
          structured += '\n'
        }
      })
      this.structured = structured
      this.relayoutAttributes()

      let nDAttr : DisplayedAttribute[] = []
      msg.inherited.forEach((attr: Attribute) => {
        if (attr.predicate === 'text') {
          this.content = attr.value
        } else if (attr.predicate !== 'parent') {
          nDAttr.push(new DisplayedAttribute(nDAttr.length, attr))
        }
      })
      this.inheritedAttributes = nDAttr
    }).fail(() => {
      this.rawContent = 'Unable to load the specified page'
    })
    this.loadContext(this.id)
  }

  deletePage (id: string) {
    if (confirm('Do you really want to delete ' + id + ' and all of its children?')) {
      $.get('/wiki/delete/' + id, () => {
        this.loadIndex()
      }).fail(() => {
        alert('Deleting the article failed.')
      })
    }
  }

  savePage () {
    let req = this.parseAttributes()
    if (this.cmEditor !== null) {
      this.rawContent = this.cmEditor.getValue()
    }
    req.push(new Attribute('text', this.rawContent, false, false, false))
    $.post('/wiki/save/' + this.id, JSON.stringify(req), (body) => {
      this.loadIndex()
      this.loadPage(this.id)
    }).fail(() => {
      alert('Saving failed.')
    })
  }

  parseAttributes () : Attribute[] {
    let attr: Attribute[] = []

    let inString = false
    let pos = 0
    while (pos < this.structured.length) {
      // skip leading whitespace
      while (pos < this.structured.length && (this.structured[pos] === ' ' || this.structured[pos] === '\n' || this.structured[pos] === '\t')) {
        pos++
      }

      // parse the predicate
      if (this.structured[pos] === '"') {
        inString = true
        ++pos
      }
      let start = pos
      while (pos < this.structured.length) {
        // Check if the word has ended (we reached non escaped whitespace)
        if (!inString && (this.structured[pos] === ' ' || this.structured[pos] === '\n' || this.structured[pos] === '\t')) {
          break
        }
        // Check if there is a string ending
        if (inString && this.structured[pos] === '"') {
          inString = false
          break
        }
        pos++
      }
      let pred = this.structured.substring(start, pos)

      // Advanve past a closing quotation mark
      if (pos < this.structured.length && this.structured[pos] === '"') {
        ++pos
      }

      // skip whitespace
      while (pos < this.structured.length && (this.structured[pos] === ' ' || this.structured[pos] === '\n' || this.structured[pos] === '\t')) {
        pos++
      }

      // parse the value
      if (this.structured[pos] === '"') {
        inString = true
        ++pos
      }
      start = pos
      while (pos < this.structured.length) {
        // Check if the word has ended (we reached non escaped whitespace)
        if (!inString && (this.structured[pos] === ' ' || this.structured[pos] === '\n' || this.structured[pos] === '\t')) {
          break
        }
        // Check if there is a string ending
        if (inString && this.structured[pos] === '"') {
          inString = false
          break
        }
        pos++
      }
      let value = this.structured.substring(start, pos)
      // Advanve past a closing quotation mark
      if (pos < this.structured.length && this.structured[pos] === '"') {
        ++pos
      }
      let prefix = pred.substr(0, 3)
      let isInteresting = prefix.includes('!')
      let isInheritable = prefix.includes('+')
      let isDate = prefix.includes('-')
      let prefixSize = 0
      prefixSize += isInteresting ? 1 : 0
      prefixSize += isInheritable ? 1 : 0
      prefixSize += isDate ? 1 : 0
      pred = pred.substr(prefixSize)
      attr.push(new Attribute(pred, value, isInteresting, isInheritable, isDate))
    }
    return attr
  }

  loadPage (id: string) {
    this.currentPage = CurrentPage.VIEW
    this.id = id
    this.entryName = ''
    $.get('/wiki/get/' + id, (body) => {
      let msg = JSON.parse(body)
      let nDAttr: DisplayedAttribute[] = []
      this.entryName = msg.name
      msg.direct.forEach((attr: Attribute) => {
        if (attr.predicate === 'text') {
          this.content = attr.value
        } else if (attr.predicate !== 'parent') {
          nDAttr.push(new DisplayedAttribute(nDAttr.length, attr))
        }
      })
      this.displayedAttributes = nDAttr
      nDAttr = []
      msg.inherited.forEach((attr: Attribute) => {
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
    this.loadContext(id)
  }

  loadContext (id : string) {
    $.get('/wiki/context/' + id, (body) => {
      let msg = JSON.parse(body)
      let nCE : ContextEntry[] = []
      msg.forEach((e: any) => {
        let c = new ContextEntry()
        c.idx = nCE.length
        c.name = e.name
        e.attributes.forEach((a: Attribute) => {
          let da = new DisplayedAttribute(c.data.length, a)
          c.data.push(da)
        })
        nCE.push(c)
      })
      this.contextEntries = nCE
    }).fail(() => {
      let c = new ContextEntry()
      c.idx = 0
      c.name = 'Unable to load the context'
      this.contextEntries = [c]
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
        event.preventDefault()
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
    let completeAttrRef = false
    for (let pos = 0; pos < MAX_CTX_SIZE && !(c.line < 0 && c.ch < 0); ++pos) {
      let line = cm.getLine(c.line)
      let char = line[c.ch]
      if (char === ')' || char === ']') {
        break
      }
      if (char === '[') {
        completeAttrRef = true
        break
      }
      if (char !== undefined) {
        // We read at least 16 characters and are on a word boundary
        if (pos > CTX_TARGET_SIZE && (char === ' ' || char === '\r' || char === '\n')) {
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
        // Setup the next character we read
        c.ch = cm.getLine(c.line).length - 1

        // Update the ctx
        ctx = '\n' + ctx
        pos++
        if (pos >= MAX_CTX_SIZE) {
          // we are done
          c.line = -1
          c.ch = -1
          break
        }
      }
    }
    // Strip the ctx from leading newlines
    let firstNonNl = 0
    for (let pos = 0; pos < ctx.length; ++pos) {
      firstNonNl = pos
      if (ctx[pos] !== '\n') {
        break
      }
    }
    ctx = ctx.substring(firstNonNl, ctx.length)

    let completionEndpoint = '/wiki/complete/'
    let completionPostfix = ''

    if (completeAttrRef) {
      completionEndpoint += 'reference'
      completionPostfix = ']'
    } else {
      completionEndpoint += 'entry'
    }
    let hints = { list: [], from: start, to: end } as CodeMirror.Hints
    return new Promise((resolve, reject) => {
      $.post(completionEndpoint, JSON.stringify({ context: ctx }), (result) => {
        result.forEach((hint: any) => {
          hints.list.push({
            text: hint.value + completionPostfix,
            displayText: hint.name + ' (' + hint.replaces + ')'
          })
        })
        resolve(hints)
      }).fail(() => {
        reject(new Error('Unable to contact the server'))
      })
    })
  }

  autoLink (id: string) {
    let didConfirm = false
    if (this.currentPage === CurrentPage.VIEW) {
      if (confirm('Do you wish to save and close this article and apply autolinking?')) {
        this.savePage()
        didConfirm = true
      } else {
        return
      }
    }
    if (didConfirm || confirm('Are you sure you want to automatically create links in this entry?')) {
      $.get('/wiki/autolink/' + id, (body) => {
        if (this.currentPage !== CurrentPage.VIEW) {
          this.loadPage(id)
        } else {
          alert('Unable to show the autolink results: you are currently editing an article.')
        }
      }).fail(() => {
        alert('Unable to autolink ' + id)
      })
    }
  }

  autoLinkAll () {
    let didConfirm = false
    if (this.currentPage === CurrentPage.VIEW) {
      if (confirm('Do you wish to save and close this article and apply autolinking?')) {
        this.savePage()
        didConfirm = true
      } else {
        return
      }
    }
    if (didConfirm || confirm('Are you sure you want to automatically create links in all entries?')) {
      $.get('/wiki/autolink', (body) => {
        alert('Autolinked all articles.')
      }).fail(() => {
        alert('An error occured while autolinking.')
      })
    }
  }

  mounted () {
    this.loadIndex()
  }

  async initCodeMirror () {
    this.currentPage = CurrentPage.EDIT
    if (this.rawContentElem === null) {
      this.rawContentElem = document.getElementById('wiki-raw-content') as (HTMLTextAreaElement | null)
      if (this.rawContentElem === null) {
        console.log('unable to initialize codemirror')
        return
      }
    }
    while (this.rawContentElem.offsetParent === null || this.rawContentElem.clientHeight === 0 || this.rawContentElem.clientWidth === 0) {
      await new Promise(resolve => setTimeout(resolve, 150))
    }
    if (this.cmEditor === null) {
      if (this.rawContentElem !== null) {
        this.cmEditor = CodeMirror.fromTextArea(this.rawContentElem, {
          lineNumbers: true,
          theme: 'panda-syntax',
          mode: 'markdown',
          extraKeys: {
            'Space': (cm: CodeMirror.Editor) => {
              cm.showHint({
                hint: this.autocompleteContent,
                completeSingle: false,
                closeCharacters: new RegExp('\\S')
              })
              cm.replaceSelection(' ')
            }
          }
        })

        // The typing for cmEditor only supports setting the base options.
        // To circumvent that we cast cme to any, and then operate on that
        // object
        let cme = this.cmEditor as any
        cme.setOption('rulers', [{ column: 100 }])

        this.cmEditor.on('changes', this.onTextChanged)
      } else {
        console.log('Warning: Unable to initialize code mirror, as the required text area was not found')
      }
    }
  }

  onTextChanged () {
    if (this.cmEditor !== null) {
      this.rawContent = this.cmEditor.getValue()
    }
  }

  relayoutAttributes (allowTrailing: boolean = false) {
    let val = this.structured
    if (val.length > 0) {
      let words = []
      let start = 0
      let inString = false
      let firstNonWs = 0
      // find the first non whitespace character
      while (firstNonWs < val.length) {
        let c = val[firstNonWs]
        let isWs = c === ' ' || c === '\r' || c === '\n'
        if (!isWs) {
          break
        }
        firstNonWs++
      }
      let insideWs = false
      // Split the input into words
      for (let pos = firstNonWs; pos < val.length; ++pos) {
        let c = val[pos]
        // If the character is a " we want to toggle the string state and also
        // process the " as a normal character
        if (c === '"') {
          inString = !inString
        }
        let isWs = c === ' ' || c === '\r' || c === '\n'
        if (!insideWs && !inString && isWs) {
          // Insert the word into the list
          words.push(val.substring(start, pos))
          insideWs = true
        } else if (insideWs && (inString || !isWs)) {
          // We transitioned from whitespace to non whitespace
          insideWs = false
          start = pos
        }
      }
      if (!insideWs) {
        words.push(val.substring(start))
      }
      let newVal = ''
      let maxlen = 0
      for (let pos = 0; pos < words.length; pos += 2) {
        maxlen = Math.max(words[pos].length, maxlen)
      }

      // determine wether we want trailing whitespace
      let end = val[val.length - 1]
      let endIsWs = end === ' ' || end === '\r' || end === '\n'
      let endMatches = (words.length % 2 === 0) && end === '\n'

      for (let pos = 0; pos < words.length; ++pos) {
        newVal += words[pos]
        if (pos + 1 < words.length || (allowTrailing && !inString && endIsWs && !endMatches)) {
          if (pos % 2 === 0) {
            newVal += ' '.repeat(maxlen - words[pos].length + 2)
          } else {
            newVal += '\n'
          }
        }
      }
      if (inString) {
        newVal += '"'
      }
      this.structured = newVal
    }
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
        this.relayoutAttributes()
      } else if (input === ' ' || input === '\t') {
        this.relayoutAttributes(true)
      }
    } else {
      console.log('Unable to find the wiki-structured element')
    }
    // TODO: Autocomplete (switch over to code mirror)
  }

  @Watch('id')
  onIdChanged (value: string, old: string) {
    let r = /[^a-zA-Z0-9_-]/
    if (r.test(value)) {
      this.id = value.replace(/[^a-zA-Z0-9_-]/, '_')
    }
  }

  loadIndex () {
    $.get('/wiki/list', (body) => {
      interface ServerIndexItem {
        name: string
        id: string
        children: ServerIndexItem[]
      }

      class DfsLevel {
        childIndex: number = 0
        item: ServerIndexItem
        idxItem: IndexTreeItem

        constructor (item: ServerIndexItem, idxItem: IndexTreeItem) {
          this.item = item
          this.idxItem = idxItem
        }
      }

      let ids = JSON.parse(body)
      if (ids === null) {
        this.indexTree = new IndexTreeItem()
        return
      }

      let idxItems = ids as ServerIndexItem
      let nIdxTree = new IndexTreeItem()
      nIdxTree.id = 'root'
      nIdxTree.html = idxItems.name
      nIdxTree.html += '<span style="width: 25px; display: inline-block"></span>'
      nIdxTree.html += '<span data-event="new" data-payload="">' + '+' + '</span>'
      nIdxTree.html += '<span style="width: 7px; display: inline-block"></span>'
      nIdxTree.html += '<span data-event="autoLinkAll" data-payload="">' + 'L' + '</span>'
      // Dfs on the tree
      let stack : DfsLevel[] = [new DfsLevel(idxItems, nIdxTree)]
      while (stack.length > 0) {
        let l = stack[stack.length - 1]
        if (l.childIndex >= l.item.children.length) {
          // we are done with this node
          if (stack.length === 1) {
            // we are done with the root node
            break
          }
          stack.pop()
        } else {
          // the node has more children, add the next child to the stack
          let child = l.item.children[l.childIndex]
          l.childIndex++
          let idx = new IndexTreeItem()
          idx.id = child.id
          idx.html = '<span data-event="show" data-payload="' + child.id + '">' + child.name + '</span>'
          idx.html += '<span style="width: 25px; display: inline-block"></span>'
          idx.html += '<span data-event="new" data-payload="' + child.id + '">' + '+' + '</span>'
          idx.html += '<span style="width: 7px; display: inline-block"></span>'
          idx.html += '<span data-event="edit" data-payload="' + child.id + '">' + 'e' + '</span>'
          idx.html += '<span style="width: 7px; display: inline-block"></span>'
          idx.html += '<span data-event="delete" data-payload="' + child.id + '">' + '-' + '</span>'
          idx.html += '<span style="width: 7px; display: inline-block"></span>'
          idx.html += '<span data-event="autoLink" data-payload="' + child.id + '">' + 'L' + '</span>'
          l.idxItem.children.push(idx)
          let cl = new DfsLevel(child, idx)
          stack.push(cl)
        }
      }
      console.log('updating the index tree')
      this.indexTree = nIdxTree
    })
  }

  showFind () {
    this.currentPage = CurrentPage.FIND
  }

  showTimeline () {
    this.currentPage = CurrentPage.TIMELINE
    $.get('/wiki/timeline', (resp: any) => {
      let data = JSON.parse(resp)
      this.timeline = data.events as TimelineEvent[]
      let color = 2
      this.timeline.forEach((event: TimelineEvent) => {
        if (event.firstDifferentField < 3) {
          color = (color + 1) % 3
        }
        event.color = color
      })
    }).fail(() => {
      alert('unable to load the timeline')
    })
  }
}
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
div .wiki-sidebar {
  position: absolute;
  width: 300px;
  top: 0px;
  bottom: 0px;
  left: 0px;
  padding-left: 5px;
  padding-right: 5px;
  border-right: 3px solid black;
  overflow: auto;
}

div .wiki-sidebar button {
  margin-top: 17px;
  width: 90px;
}

div .wiki-center {
  overflow-y: auto;
  position: absolute;
  left: 310px;
  top: 0px;
  bottom: 0px;
  right: 310px;
  padding: 7px;
}

div .wiki-sidebar-right {
  position: absolute;
  width: 300px;
  top: 0px;
  bottom: 0px;
  right: 0px;
  padding-left: 5px;
  padding-right: 5px;
  border-left: 3px solid black;
  overflow: auto;
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

td {
  padding-right: 25px;
}

.interesting-attr {
  font-weight: bold;
}

.inheritable-attr {
  font-style: italic;
}

.timeline-hr td {
  border-bottom: 1px solid white;
}

.timeline-color1 {
  border-left: 3px solid green;
}

.timeline-color2 {
  border-left: 3px solid yellow;
}

.timeline-color3 {
  border-left: 3px solid red;
}

#wiki-timeline td {
  padding-top: 0px;
  padding-bottom: 0px;
  padding-left: 7px;
}

#wiki-timeline table {
  border-collapse: collapse;
}

</style>

<style>
table p {
 margin: 0px;
}

.CodeMirror {
  font-family: Avenir,Helvetica,Arial,sans-serif !important;
}
</style>
