#include "EventBus.h"

namespace atlas {

EventBus::EventBus() {}

void EventBus::listenTo(const std::string &event, EventHandler handler) {
  _handlers[event].push_back(handler);
}
void EventBus::trigger(const std::string &event) {
  auto it = _handlers.find(event);
  if (it != _handlers.end()) {
    for (EventHandler &f : it->second) {
      f(event);
    }
  }
}

}  // namespace atlas
