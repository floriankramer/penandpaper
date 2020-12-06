/**
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
 */
#pragma once

#include <memory>
#include <mutex>
#include <string>
#include <unordered_map>

#include "Database.h"

class UserManager {
  static const std::string COL_ID;
  static const std::string COL_NAME;
  static const std::string COL_SALT;
  static const std::string COL_PASSWORD;
  static const std::string COL_OAUTH;
  static const std::string COL_OAUTH_EXPIRY;
  static const std::string COL_PERMISSIONS;
  static const std::string COL_UID;

  static const std::string COL_KEY;
  static const std::string COL_VALUE;

 public:
  enum class Permission { MODIFY_USERS, ADMIN };

  class User {
   public:
    User(Database *db, DbTable *table,
         std::shared_ptr<std::recursive_mutex> auth_mutex, DbCursor &db_entry);

    virtual ~User();

    const std::string &name() const;
    void setName(const std::string &name);

    void setPassword(const std::string &password_hash);

    void setPermissions(const std::vector<Permission> &new_permissions);
    /**
     * @brief Sets the permissions bit mask directly
     */
    void setPermissionsRaw(int64_t permissions);

    bool hasPermission(Permission permission) const;

    const std::string &oauth() const;
    void refreshOauth();
    std::string createSetCookieHeader() const;
    std::string createClearCookieHeader() const;

    int64_t id() const;
    const std::string &uid() const;

    void onDeleted(Database *db);

    bool isDeleted() const;

   private:
    int64_t _id;
    std::string _name;
    std::string _salt;
    std::string _password_hash;
    std::string _oauth;
    int64_t _oauth_expiry;
    int64_t _permissions;
    std::string _uid;

    mutable std::shared_ptr<std::recursive_mutex> _auth_mutex;

    DbTable *_user_table;
    DbTable _data_table;

    bool _is_deleted;
  };
  using UserPtr = std::shared_ptr<User>;

  /**
   * @brief Contains data about users thats available to any logged in user
   */
  struct PublicUserInfo {
    int64_t id;
    std::string name;
    int64_t permissions;
  };

  UserManager(Database *db);
  virtual ~UserManager();

  UserPtr authenticateViaLogin(const std::string &username,
                               const std::string &password);
  UserPtr authenticateViaOAuth(const std::string &oauth);
  UserPtr authenticateViaCookies(const std::string &cookies);

  UserPtr getAuthenticatedUser(const std::string &oauth);
  UserPtr loadUser(int64_t id);
  UserPtr createUser(const std::string &username,
                     const std::string &password_hash,
                     const std::vector<Permission> &permissions);

  void deleteUser(int64_t id);

  std::vector<PublicUserInfo> listUsers();

  static std::string clientSidePasswordHash(const std::string &username,
                                            const std::string &password);
  static std::string hashPassword(const std::vector<char> &salt,
                                  const std::string &password);
  static std::vector<char> generateSalt();

 private:
  // Maps oauth to users
  std::unordered_map<std::string, UserPtr> _authenticated_users;
  std::unordered_map<int64_t, UserPtr> _loaded_users;
  mutable std::shared_ptr<std::recursive_mutex> _auth_mutex;

  Database *_db;
  DbTable _users;

  bool _can_authenticate_users;
};
