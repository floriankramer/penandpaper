import Vue from 'vue'
import Vuex from 'vuex'

Vue.use(Vuex)

export default new Vuex.Store({
  state: {
    username: '',
    permissions: 0
  },
  mutations: {
    setUsername (state, newname) {
      console.log('Setting the username to ' + newname)
      state.username = newname
    },
    setPermissions (state, newperms) {
      state.permissions = newperms
    }
  },
  actions: {
  },
  modules: {
  }
})
