// Debug code
util = (function() {
  socket = null;
  onmessage = null;

  function setOnmessage(fn) {
    onmessage = fn;
    if (socket != null) {
      socket.onmessage = fn;
    }
  }

  function connect() {
    port = ''
    if (window.location.port != null) {
      port = ':' + window.location.port
    }
    socket = new WebSocket('wss://' + window.location.hostname + port);
    socket.onmessage = onmessage;
  }

  function send(data) {
    socket.send(data);
  }

  function disconnect() {
    socket.close();
    socket = null;
  }

  return {
    connect : connect,
    disconnect : disconnect,
    send : send,
    setOnmessage : setOnmessage
  };
})();
