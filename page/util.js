// Debug code
(function() {
  ws = new WebSocket('wss://localhost:8080');
  ws.onopen = function () {
    ws.send("A test message.");
  }
  ws.onmessage = function(event) {
    console.log("received a message: " + event.data);
  }
})();
