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
#include "UserManager.h"

#include <sodium.h>

#include <cstdlib>
#include <sstream>

#include "Logger.h"
#include "Random.h"
#include "Util.h"

UserManager::UserManager(Database *db)
    : _auth_mutex(std::make_shared<std::recursive_mutex>()),
      _db(db),
      _users(db->createTable("users", {{"id", DbDataType::AUTO_INCREMENT},
                                       {"name", DbDataType::TEXT},
                                       {"salt", DbDataType::TEXT},
                                       {"password", DbDataType::TEXT},
                                       {"oauth", DbDataType::TEXT},
                                       {"oauth_expiry", DbDataType::INTEGER},
                                       {"permissions", DbDataType::INTEGER}})),
      _can_authenticate_users(true) {
  if (sodium_init() < 0) {
    LOG_ERROR << "UserManager::UserManager: Unable to initialize libsodium, "
                 "unable to provide user authentification."
              << LOG_END;
    _can_authenticate_users = false;
  }
  DbCursor root_users = _users.query();

  if (root_users.done()) {
    // compute the hash of roots password
    std::vector<char> salt = generateSalt();
    std::string client_side_hash = clientSidePasswordHash("root", "root");
    LOG_DEBUG << "Client side password " << client_side_hash << LOG_END;
    LOG_DEBUG << "Salt hex " << util::base16EncodeStr(salt.data(), salt.size())
              << LOG_END;
    LOG_DEBUG << "Salt " << util::base64EncodeStr(salt.data(), salt.size())
              << LOG_END;
    std::string password_hash = hashPassword(salt, client_side_hash);
    LOG_DEBUG << "Entry to write " << password_hash << LOG_END;

    _users.insert({DbColumnUpdate{"name", std::string("root")},
                   DbColumnUpdate{
                       "salt", util::base64EncodeStr(salt.data(), salt.size())},
                   DbColumnUpdate{"password", password_hash},
                   DbColumnUpdate{"oauth", std::string("")},
                   DbColumnUpdate{"oauth_expiry", int64_t(0)},
                   DbColumnUpdate{"permissions", int64_t(0xFFFFFFF)}});
  }
}

UserManager::~UserManager() {}

UserManager::UserPtr UserManager::authenticateViaLogin(
    const std::string &username, const std::string &password) {
  std::lock_guard<std::recursive_mutex> auth_lock(*_auth_mutex);
  if (!_can_authenticate_users) {
    LOG_WARN
        << "UserManager::authenticateViaLogin: Unable to authenticate any users"
        << LOG_END;
    return nullptr;
  }
  LOG_DEBUG << "Querying for users with the given name" << LOG_END;
  DbCursor users =
      _users.query(DbCondition("name", DbCondition::Type::EQ, username));
  LOG_DEBUG << " Query complete" << LOG_END;
  while (!users.done()) {
    LOG_DEBUG << "UserManager::authenticateViaLogin: trying "
              << users.col(1).text << LOG_END;
    std::string salt = users.col(2).text;
    std::vector<char> decoded_salt =
        util::base64Decode(salt.c_str(), salt.size());
    LOG_DEBUG << "Salt hex "
              << util::base16EncodeStr(decoded_salt.data(), decoded_salt.size())
              << LOG_END;
    LOG_DEBUG << "Hashing the password using the salt: " << salt << LOG_END;
    std::string passwd_hash =
        hashPassword(util::base64Decode(salt.c_str(), salt.size()), password);
    LOG_DEBUG << "Hashed password " << passwd_hash
              << " db password: " << users.col(3).text << LOG_END;
    if (passwd_hash == users.col(3).text) {
      LOG_DEBUG << "The password matches" << LOG_END;
      UserPtr user = std::make_shared<User>(&_users, _auth_mutex, users);
      auto user_it = _authenticated_users.find(user->oauth());
      if (user_it != _authenticated_users.end()) {
        return user_it->second;
      } else {
        user->refreshOauth();
        return user;
      }
    }
    LOG_DEBUG << "The password didn't match" << LOG_END;
    users.next();
  }
  return nullptr;
}

UserManager::UserPtr UserManager::authenticateViaOAuth(
    const std::string &oauth) {
  std::lock_guard<std::recursive_mutex> auth_lock(*_auth_mutex);
  if (!_can_authenticate_users) {
    return nullptr;
  }
  auto user_it = _authenticated_users.find(oauth);
  if (user_it != _authenticated_users.end()) {
    return user_it->second;
  }
  int64_t now = time(NULL);
  DbCursor users =
      _users.query(DbCondition("oauth", DbCondition::Type::EQ, oauth));
  while (!users.done()) {
    if (users.col(5).integer > now) {
      UserPtr user = std::make_shared<User>(&_users, _auth_mutex, users);
      user->refreshOauth();
      _authenticated_users.insert(std::make_pair(oauth, user));
      _loaded_users.insert(std::make_pair(user->id(), user));
      return user;
    }
    users.next();
  }
  return nullptr;
}

UserManager::UserPtr UserManager::authenticateViaCookies(
    const std::string &cookies) {
  std::lock_guard<std::recursive_mutex> auth_lock(*_auth_mutex);
  if (!_can_authenticate_users) {
    return nullptr;
  }
  size_t pos = cookies.find("auth=");
  if (pos == std::string::npos) {
    return nullptr;
  }
  pos += 5;
  size_t end = cookies.find(';', pos);
  if (end == std::string::npos) {
    end = cookies.size();
  }
  std::string token = cookies.substr(pos, end - pos);
  return authenticateViaOAuth(token);
}

UserManager::UserPtr UserManager::getAuthenticatedUser(
    const std::string &oauth) {
  std::lock_guard<std::recursive_mutex> auth_lock(*_auth_mutex);
  auto user_it = _authenticated_users.find(oauth);
  if (user_it == _authenticated_users.end()) {
    return nullptr;
  }
  return user_it->second;
}

UserManager::UserPtr UserManager::loadUser(int64_t id) {
  std::lock_guard<std::recursive_mutex> auth_lock(*_auth_mutex);
  auto loaded_it = _loaded_users.find(id);
  if (loaded_it != _loaded_users.end()) {
    return loaded_it->second;
  }

  DbCursor users = _users.query(DbCondition("id", DbCondition::Type::EQ, id));
  if (users.done()) {
    return nullptr;
  }
  UserPtr user = std::make_shared<User>(&_users, _auth_mutex, users);
  // ensure the user has an oauth token
  user->refreshOauth();
  _authenticated_users.insert(std::make_pair(user->oauth(), user));
  _loaded_users.insert(std::make_pair(user->id(), user));
  return user;
}

UserManager::UserPtr UserManager::createUser(
    const std::string &username, const std::string &password_hash,
    const std::vector<Permission> &permissions) {
  std::lock_guard<std::recursive_mutex> auth_lock(*_auth_mutex);
  {
    DbCursor users =
        _users.query(DbCondition("name", DbCondition::Type::EQ, username));
    if (!users.done()) {
      throw std::runtime_error(
          "UserManager::createUser: A user with the name " + username +
          " already exists.");
    }
  }

  std::vector<char> salt = generateSalt();
  std::string passwd = hashPassword(salt, password_hash);

  int64_t permission_mask = 0;
  for (Permission p : permissions) {
    permission_mask |= int64_t(1) << int64_t(p);
  }

  _users.insert(
      {DbColumnUpdate{"name", username},
       DbColumnUpdate{"salt", util::base64EncodeStr(salt.data(), salt.size())},
       DbColumnUpdate{"password", passwd},
       DbColumnUpdate{"oauth", std::string("")},
       DbColumnUpdate{"oauth_expiry", int64_t(0)},
       DbColumnUpdate{"permissions", permission_mask}});

  DbCursor users =
      _users.query(DbCondition("name", DbCondition::Type::EQ, username));
  UserPtr user = std::make_shared<User>(&_users, _auth_mutex, users);
  user->refreshOauth();
  _authenticated_users.insert(std::make_pair(user->oauth(), user));
  return user;
}

void UserManager::deleteUser(int64_t id) {
  std::lock_guard<std::recursive_mutex> auth_lock(*_auth_mutex);
  UserPtr user = loadUser(id);
  if (user == nullptr) {
    return;
  }
  auto authenticated_it = _authenticated_users.find(user->oauth());
  if (authenticated_it != _authenticated_users.end()) {
    _authenticated_users.erase(authenticated_it);
  }
  auto loaded_it = _loaded_users.find(user->id());
  if (loaded_it != _loaded_users.end()) {
    _loaded_users.erase(loaded_it);
  }
  _users.erase(DbCondition("id", DbCondition::Type::EQ, user->id()));
}

std::vector<UserManager::PublicUserInfo> UserManager::listUsers() {
  std::vector<PublicUserInfo> users;
  DbCursor result = _users.query();
  while (!result.done()) {
    PublicUserInfo user;
    user.id = result.col("id").integer;
    user.name = result.col("name").text;
    user.permissions = result.col("permissions").integer;
    users.push_back(user);
    result.next();
  }
  return users;
}

std::string UserManager::clientSidePasswordHash(const std::string &username,
                                                const std::string &password) {
  const std::string to_hash = username + password;
  unsigned char buffer[crypto_hash_sha256_BYTES];
  crypto_hash_sha256(buffer,
                     reinterpret_cast<const unsigned char *>(to_hash.c_str()),
                     to_hash.size());
  std::vector<char> encoded =
      util::base16Encode(buffer, crypto_hash_sha256_BYTES);
  return std::string(encoded.begin(), encoded.end());
}

std::string UserManager::hashPassword(const std::vector<char> &salt,
                                      const std::string &password) {
  unsigned char out[24];
  int r = crypto_pwhash(out, 24, password.c_str(), password.size(),
                        reinterpret_cast<const unsigned char *>(salt.data()),
                        crypto_pwhash_OPSLIMIT_MIN, crypto_pwhash_MEMLIMIT_MIN,
                        crypto_pwhash_ALG_ARGON2ID13);
  if (r != 0) {
    LOG_ERROR << "UserManager::hashPassword: out of memory, disabling "
                 "authentification.";
    return "";
  }
  return util::base64EncodeStr(out, 24);
}

std::vector<char> UserManager::generateSalt() {
  return Random::secureRandomBytes(crypto_pwhash_SALTBYTES);
}

// USER
// =============================================================================

UserManager::User::User(Table *table,
                        std::shared_ptr<std::recursive_mutex> auth_mutex,
                        DbCursor &db_entry)
    : _auth_mutex(auth_mutex), _table(table) {
  _id = db_entry.col(0).asInteger();
  _name = db_entry.col(1).asText();
  _salt = db_entry.col(2).asText();
  _password_hash = db_entry.col(3).asText();
  _oauth = db_entry.col(4).asText();
  _oauth_expiry = db_entry.col(5).asInteger();
  _permissions = db_entry.col(6).asInteger();
}

const std::string &UserManager::User::name() const {
  // TODO: This is not thread-safe.
  return _name;
}

void UserManager::User::setName(const std::string &name) {
  std::lock_guard<std::recursive_mutex> auth_lock(*_auth_mutex);
  _table->update({DbColumnUpdate{"name", name}},
                 DbCondition("id", DbCondition::Type::EQ, _id));
  _name = name;
}

void UserManager::User::setPassword(const std::string password) {
  std::lock_guard<std::recursive_mutex> auth_lock(*_auth_mutex);
  std::vector<char> salt = UserManager::generateSalt();
  std::string password_hash = UserManager::hashPassword(salt, password);
  _table->update(
      {DbColumnUpdate{"password", password_hash},
       DbColumnUpdate{"salt", util::base64EncodeStr(salt.data(), salt.size())}},
      DbCondition("id", DbCondition::Type::EQ, _id));
  _password_hash = password_hash;
}

void UserManager::User::setPermissions(
    const std::vector<Permission> &new_permissions) {
  std::lock_guard<std::recursive_mutex> auth_lock(*_auth_mutex);
  int64_t perm_mask = 0;
  for (Permission p : new_permissions) {
    perm_mask |= int64_t(1) << int64_t(p);
  }
  _table->update({DbColumnUpdate{"permissions", perm_mask}},
                 DbCondition("id", DbCondition::Type::EQ, _id));
  _permissions = perm_mask;
}

bool UserManager::User::hasPermission(Permission permission) const {
  std::lock_guard<std::recursive_mutex> auth_lock(*_auth_mutex);
  return (_permissions & (int64_t(1) << int64_t(permission))) != 0;
}

const std::string &UserManager::User::oauth() const { return _oauth; }

void UserManager::User::refreshOauth() {
  int64_t now = time(NULL);
  if (_oauth_expiry <= now) {
    _oauth = Random::secureRandomString(32);
    _table->update({DbColumnUpdate{"oauth", _oauth}},
                   DbCondition("id", DbCondition::Type::EQ, _id));
  }
  _oauth_expiry = now + 12l * 7l * 24l * 60l * 60l;
  _table->update({DbColumnUpdate{"oauth_expiry", _oauth_expiry}},
                 DbCondition("id", DbCondition::Type::EQ, _id));
}

std::string UserManager::User::createSetCookieHeader() const {
  std::ostringstream out;
  out << "auth=" << _oauth << ';';
  out << " Max-Age=" << (365 * 24 * 3600) << ';';
  out << " Secure; HttpOnly; Path=/";
  return out.str();
}

std::string UserManager::User::createClearCookieHeader() const {
  std::ostringstream out;
  out << "auth=" << ';';
  out << " Max-Age=-1" << ';';
  out << " Secure; HttpOnly; Path=/";
  return out.str();
}

int64_t UserManager::User::id() const { return _id; }
