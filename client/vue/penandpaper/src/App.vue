<template>
  <div id="app">
    <CriticalError v-if="hasCriticalError">{{criticalErrorString}}</CriticalError>
    <template v-else>
      <Login v-bind:server="server"></Login>
    </template>
  </div>
</template>

<script lang="ts">
import { Component, Vue } from 'vue-property-decorator'
import Login from './components/Login.vue'
import CriticalError from './components/CriticalError.vue'
import Server from './components/server'

@Component({
  components: {
    Login,
    CriticalError
  }
})
export default class App extends Vue {
  server:Server = new Server();

  hasCriticalError:boolean = false;
  criticalErrorString:string = '';

  mounted () {
    console.log('The app has been mounted')
    this.server.setErrorHandler(this.onConnectError)
    this.server.connect()
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
  text-align: center;
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
