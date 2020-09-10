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
 public:
  enum class Permission { CREATE_USERS };

  class User {
   public:
    User(Table *table, DbCursor &db_entry);

    const std::string &name() const;
    void setName(const std::string &name);

    void setPassword(const std::string password_hash);

    bool hasPermission(Permission permission) const;

    const std::string &oauth() const;
    void refreshOauth();
    std::string createSetCookieHeader() const;

   private:
    int64_t _id;
    std::string _name;
    std::string _salt;
    std::string _password_hash;
    std::string _oauth;
    int64_t _oauth_expiry;
    int64_t _permissions;

    Table *_table;
  };
  using UserPtr = std::shared_ptr<User>;

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

  static std::string clientSidePasswordHash(const std::string &username,
                                            const std::string &password);
  static std::string hashPassword(const std::vector<char> &salt,
                                  const std::string &password);
  static std::vector<char> generateSalt();

 private:
  // Maps oauth to users
  std::unordered_map<std::string, UserPtr> _authenticated_users;
  mutable std::mutex _auth_mutex;

  Database *_db;
  Table _users;

  bool _can_authenticate_users;
};
