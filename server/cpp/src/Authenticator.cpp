#include "Authenticator.h"

#include <sstream>

#include "Logger.h"
#include "Random.h"

Authenticator::Authenticator() {}
Authenticator::~Authenticator() {}

bool Authenticator::authenticate(const std::string &token) const {
  std::lock_guard<std::mutex> lock(_auth_mutex);
  return _authenticated_users.count(token) > 0;
}

bool Authenticator::authenticateFromCookies(const std::string &cookies) const {
  size_t pos = cookies.find("auth=");
  if (pos == std::string::npos) {
    return false;
  }
  pos += 5;
  size_t end = cookies.find(';', pos);
  if (end == std::string::npos) {
    end = cookies.size();
  }
  std::string token = cookies.substr(pos, end - pos);
  return authenticate(token);
}

std::string Authenticator::createSetCookieHeader(
    const std::string &token) const {
  std::ostringstream out;
  out << "auth=" << token << ';';
  out << " Max-Age=" << (365 * 24 * 3600) << ';';
  out << " Secure; HttpOnly; Path=/";
  return out.str();
}

std::string Authenticator::addAuthenticated(const std::string &uid) {
  std::lock_guard<std::mutex> lock(_auth_mutex);
  std::string token = Random::secureRandomString(32);
  _authenticated_users[token] = uid;
  return token;
}

std::string Authenticator::uidFromToken(const std::string &token) {
  std::lock_guard<std::mutex> lock(_auth_mutex);
  auto it = _authenticated_users.find(token);
  if (it != _authenticated_users.end()) {
    return it->second;
  }
  return "";
}
