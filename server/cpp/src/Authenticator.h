#pragma once

#include <string>
#include <unordered_map>
#include <mutex>

class Authenticator {
public:
  Authenticator();
  virtual ~Authenticator();

  /**
   * @param token The auth token of the authentication request
   * @return true if the id is in the authentication database.
   */
  bool authenticate(const std::string &token) const;

  /**
   * @brief A utility function to attempt to authenticate a user through an
   * authentication cookie.
   * @param cookies The cookies header send with the http request.
   */
  bool authenticateFromCookies(const std::string &cookies) const;

  std::string createSetCookieHeader(const std::string &token) const;

  /**
   * @brief Adds an authenticated user with the given uid. Returns an auth token
   *        for that user. Can be used to reset the users token.
   */
  std::string addAuthenticated(const std::string &uid);

  /**
   * @return The uid associated with the given token
   */
  std::string uidFromToken(const std::string &token);

private:
  // Maps tokens to uids
  std::unordered_map<std::string, std::string> _authenticated_users;
  mutable std::mutex _auth_mutex;

};
