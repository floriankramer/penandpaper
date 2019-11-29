#pragma once

#include <functional>
#include <string>
#include <vector>


#define ASIO_STANDALONE
#include <websocketpp/config/asio.hpp>
#include <websocketpp/server.hpp>

class WebSocketServer {

  typedef websocketpp::config::asio_tls ServerConfig;
  typedef websocketpp::server<websocketpp::config::asio_tls> Server;
  typedef std::shared_ptr<asio::ssl::context> ssl_ctx_pt;

public:
  enum class ResponseType {
    BROADCAST, // send new data to all
    FORWARD, // send the received message to all
    RETURN, // send to sender
    SILENCE
  };

  struct Response {
    std::string text;
    ResponseType type;
  };

  typedef std::function<Response(const std::string&)> OnMsgHandler_t;
  typedef std::function<Response()> OnConnectHandler_t;


public:
   WebSocketServer(OnMsgHandler_t on_msg, OnConnectHandler_t on_connect);

private:
   void run();

   void handleResponse(const Response &response, websocketpp::connection_hdl &initiator);

   std::vector<websocketpp::connection_hdl> _connections;

   Server _socket;

   OnMsgHandler_t _on_msg;
   OnConnectHandler_t _on_connect;

};
