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
  <div class="container">
    <h1>Users</h1>
    <details>
      <input placeholder="name" v-model="newName"/>
      <input placeholder="password" type="password" v-model="newPassword"/>
      <input type="checkbox" value="Modify Users" name="perm-modify-uysers" v-model="newModifyUsers"/>
      <label for="perm-modify-uysers">Modify Users</label>
      <input type="checkbox" value="Admin" name="perm-admin" v-model="newAdmin"/>
      <label for="perm-modify-uysers">Admin</label>
      <button v-on:click="createUser">Create User</button>
    </details>
    <table>
      <thead>
        <tr>
          <th>Id</th>
          <th>Name</th>
          <th>Password</th>
          <th>Permissions</th>
          <th>Delete User</th>
        </tr>
      </thead>
      <tr v-for="user in users" v-bind:key="user.id">
        <td>{{user.id}}</td>
        <td>{{user.name}}</td>
        <td v-on:click="editPassword(user.id, user.name)">&bull;&bull;&bull;&bull;&bull;</td>
        <td v-on:click="editPermissions(user.id)">
          <span v-for="permission in user.permissions" v-bind:key="permission">
            {{permission}}
          </span>
        </td>
        <td>
          <button v-on:click="deleteUser(user.id, user.name)">delete</button>
        </td>
      </tr>
    </table>
    <div class="popup" v-show="showEditPassword">
      <input placeholder="password" type="password" v-model="editedPassword"/>
      <button v-on:click="commitPassword">Save</button>
      <button v-on:click="cancelChangePassword">Cancel</button>
    </div>
    <div class="popup" v-show="showEditPermissions">
      <input type="checkbox" name="perm-modify-uysers" value="Modify Users" v-model="editedModifyUsers"/>
      <label for="perm-modify-uysers">Modify Users</label>
      <input type="checkbox" name="perm-admin" value="Admin" v-model="editedAdmin"/>
      <label for="perm-modify-uysers">Admin</label>
      <button v-on:click="commitPermissions">Save</button>
      <button v-on:click="cancelChangePermissions">Cancel</button>
    </div>
  </div>
</template>

<script lang="ts">

import { Component, Prop, Vue } from 'vue-property-decorator'
import Server from './server'
import eventBus from '../eventbus'

import $ from 'jquery'

class User {
  id: number = 0
  name: string = ''
  permissions: string[] = []
}

@Component({
  components: {
  }
})
export default class UserManager extends Vue {
  users: User[] = []

  showEditPassword: boolean = false
  showEditPermissions: boolean = false

  currentUserName: string = ''
  currentId: number = 0

  newName: string = ''
  newPassword: string = ''
  newModifyUsers: boolean = false
  newAdmin: boolean = false

  editedAdmin: boolean = false
  editedModifyUsers: boolean = false

  editedPassword: string = ''

  mounted () {
    this.updateUsersList()
  }

  updateUsersList () {
    $.get('/auth/list', (data: any) => {
      let newUsers: User[] = []
      data.users.forEach((user: any) => {
        let permissions: string[] = []
        newUsers.push({
          'id': user.id,
          'name': user.name,
          'permissions': user.permissions
        })
      })
      console.log('new users', newUsers)
      this.users = newUsers
    })
  }

  editPassword (id: number, name: string) {
    this.showEditPassword = true
    this.showEditPermissions = false
    this.currentId = id
    this.currentUserName = name
  }

  commitPassword () {
    this.showEditPassword = false
    this.editedPassword = ''

    this.hashPassword(this.currentUserName, this.editedPassword).then((hashedPassword: string) => {
      let req: any = {
        'action': 'SetPassword',
        'to-modify': this.currentId,
        'new-password': hashedPassword
      }

      $.post('/auth', JSON.stringify(req), () => {
        eventBus.$emit('/notification', 'Updated your password')
      }).fail(() => {
        eventBus.$emit('/notification', 'Unable to change the password')
      })
    }).catch(() => {
      eventBus.$emit('/notification', 'Unable to change the password')
    })
  }

  cancelChangePassword () {
    this.showEditPassword = false
  }

  editPermissions (id: number) {
    this.showEditPassword = false
    this.showEditPermissions = true
    this.currentId = id

    let user = this.users.find((user: User) => user.id === id)
    if (user !== undefined) {
      this.editedModifyUsers = user.permissions.find((perm) => perm === 'modify-users') !== undefined
      this.editedAdmin = user.permissions.find((perm) => perm === 'admin') !== undefined
    } else {
      this.editedModifyUsers = false
      this.editedAdmin = false
    }
  }

  commitPermissions () {
    this.showEditPermissions = false

    let perms = []
    if (this.editedAdmin) {
      perms.push('admin')
    }
    if (this.editedModifyUsers) {
      perms.push('modify-users')
    }
    let req: any = {
      'action': 'SetPermissions',
      'to-modify': this.currentId,
      'new-permissions': perms
    }

    $.post('/auth', JSON.stringify(req), () => {
      eventBus.$emit('/notification', 'Updated the permissions')
      this.updateUsersList()
    }).fail(() => {
      eventBus.$emit('/notification', 'Unable to change the permissions')
    })
  }

  cancelChangePermissions () {
    this.showEditPermissions = false
  }

  deleteUser (id: number, name: string) {
    if (!confirm('Do you really want to delete user ' + name + '?')) {
      return
    }
    let req: any = {
      'action': 'DeleteUser',
      'to-delete': id
    }
    $.post('/auth', JSON.stringify(req), () => {
      eventBus.$emit('/notification', 'Deleted ' + name)
      this.updateUsersList()
    }).fail(() => {
      eventBus.$emit('/notification', 'Unable to delete user ' + name)
    })
  }

  async createUser () {
    let permissions = []
    if (this.newModifyUsers) {
      permissions.push('modify-users')
    }
    if (this.newAdmin) {
      permissions.push('admin')
    }

    let req: any = {
      'action': 'CreateUser',
      'new-name': this.newName,
      'new-password': await this.hashPassword(this.newName, this.newPassword),
      'new-permissions': permissions
    }
    $.post('/auth', JSON.stringify(req), () => {
      this.newName = ''
      this.newPassword = ''
      this.newModifyUsers = false
      this.newAdmin = false
      eventBus.$emit('/notification', 'Created a new user')
      this.updateUsersList()
    }).fail(() => {
      eventBus.$emit('/notification', 'Unable to create the new user')
    })
  }

  async hashPassword (name: string, password: string): Promise<string> {
    const passwordUtf8 = new TextEncoder().encode(name + password)
    let passwordHashBytes = await crypto.subtle.digest('SHA-256', passwordUtf8)
    let passwordArray = Array.from(new Uint8Array(passwordHashBytes))
    let passwordHex = passwordArray.map(x => ('00' + x.toString(16)).slice(-2)).join('').toUpperCase()
    return passwordHex
  }
}
</script>

<style scoped>
div.container {
  padding: 20px;
}

table {
  width: 100%;
}

.popup {
  width: 500px;
  height: 300px;
  padding: 20px;
  border-radius: 10px;
  position: absolute;
  top: 50%;
  left: 50%;
  transform: translate(-50%, -50%);
}

body.light-mode .popup {
  background-color: #434343;
}

body.dark-mode .popup {
  background-color: #333;
}
</style>
