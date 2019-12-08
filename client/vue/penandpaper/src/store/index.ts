import Vue from 'vue'
import Vuex from 'vuex'

Vue.use(Vuex)

export default new Vuex.Store({
  state: {
    username: ''
  },
  mutations: {
    setUsername (state, newname) {
      console.log('Setting the username to ' + newname)
      state.username = newname
    }
  },
  actions: {
  },
  modules: {
  }
})
