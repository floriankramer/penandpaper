#pragma once

#include <functional>
#include <string>
#include <unordered_map>
#include <vector>

namespace atlas {

class EventBus {
 public:
  typedef std::function<void(const std::string &)> EventHandler;
  EventBus();

  void listenTo(const std::string &event, EventHandler handler);
  void trigger(const std::string &event);

 private:
  std::unordered_map<std::string, std::vector<EventHandler>> _handlers;
};

}  // namespace atlas
