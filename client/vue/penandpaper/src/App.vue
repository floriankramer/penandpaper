<template>
  <div id="app">
    <CriticalError v-if="hasCriticalError">{{criticalErrorString}}</CriticalError>
    <template v-else-if="hasUsername">
      <div class="topbar">
        <Toolbar></Toolbar>
      </div>
      <div class="sidebar">
        <Chat></Chat>
      </div>
      <Map class="content-area"/>
    </template>
    <template v-else>
      <Login v-bind:server="server"></Login>
    </template>
  </div>
</template>

<script lang="ts">
import { Component, Vue } from 'vue-property-decorator'
import Login from './components/Login.vue'
import Chat from './components/Chat.vue'
import Toolbar from './components/Toolbar.vue'
import Map from './components/Map.vue'
import CriticalError from './components/CriticalError.vue'
import Server from './components/server'
import { MutationPayload } from 'vuex'

@Component({
  components: {
    Login,
    CriticalError,
    Chat,
    Toolbar,
    Map
  }
})
export default class App extends Vue {
  server:Server = new Server(this.$store);

  hasCriticalError:boolean = false;
  criticalErrorString:string = '';

  hasUsername: boolean = false;

  mounted () {
    console.log('The app has been mounted')
    this.server.setErrorHandler(this.onConnectError)
    this.server.connect()

    let app = this
    this.$store.subscribe((mutation: MutationPayload, state: any) => {
      if (mutation.type === 'setUsername') {
        if (state.username.length > 0) {
          app.hasUsername = true
        }
      }
    })
  }

  onConnectError () {
    this.hasCriticalError = true
    this.criticalErrorString = 'Lost connection to the server.'
  }
}
</script>

<style>
body {
  font-family: 'Avenir', Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  color: white;
  background-color: rgb(51, 51, 51);
  font-size: 14pt;
}

input {
  background-color: rgb(100, 100, 100);
  color: white;
  border: none;
  border-radius: 5px;
  padding: 3px;
}

button {
  background-color: rgb(100, 100, 100);
  color: white;
  border: none;
  border-radius: 5px;
  padding: 3px;
  padding-right: 20px;
  padding-left: 20px;
}

#app {
  top: 0px;
  left: 0px;
  bottom: 0px;
  right: 0px;
  overflow: hidden;
  position: absolute;
}

div .sidebar {
  width: 350px;
  position: absolute;
  top: 0px;
  bottom: 0px;
  right: 0px;
  background-color: rgb(51, 51, 51);
  color: white;
  box-shadow: -3px 0px 5px black;
  padding-left: 10px;
  padding-right: 10px;
}

div .topbar {
  position: absolute;
  left: 0px;
  top: 0px;
  height: 40px;
  right: 380px;
  border-bottom: 2px solid black;
}

.content-area {
  position: absolute;
  top: 42px;
  left: 0px;
  bottom: 0px;
  right: 380px;
}

/** Image radio buttons: https://stackoverflow.com/questions/17541614/use-images-instead-of-radio-buttons*/
/* HIDE RADIO */
[type=radio] {
  position: absolute;
  opacity: 0;
  width: 0;
  height: 0;
  left: -100%;
}

/* IMAGE STYLES */
[type=radio] + img {
  cursor: pointer;
  margin-right: 10px;
}

/* CHECKED STYLES */
[type=radio]:checked + img {
  border: 1px solid rgb(200, 200, 200);
  border-radius: 1000px;
}
</style>
