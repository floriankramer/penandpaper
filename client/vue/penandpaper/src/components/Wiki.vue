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
      <button>new</button>
      <button>edit</button>
      <button>delete</button>
      <button>home</button>
    </div>
    <div class="wiki-center">
      <div id="wiki-content" v-if="showContent">
        The page '' does not exist yet.
      </div>
      <div id="wiki-edit" v-if="showEdit">
        <form>
          <div class="with-tooltip">
            <label for="title">id: </label>
            <input name="title" v-bind:value="title"/>
            <span class="tooltip">The unique identifier of this article.</span>
          </div>
          <div>
            <textarea name="content" rows="36" cols="80" v-bind:value="content"/>
          </div>
          <div>
            <input type="submit" value="save"/>
          </div>
        </form>
      </div>
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator'
import Server from './server'

@Component
export default class Wiki extends Vue {
  showContent: boolean = false
  showEdit: boolean = true

  title: string = 'home'
  content: string = 'Lorem Ipsum...'
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
</style>
