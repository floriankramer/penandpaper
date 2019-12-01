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

  function connect(onopen = null) {
    port = ':8081'
    //if (window.location.port != null) {
    //  port = ':' + window.location.port
    //}
    socket = new WebSocket('wss://' + window.location.hostname + port);
    socket.onmessage = onmessage;
    if (onopen) {
      socket.onopen = onopen
    }
  }

  function send(data) {
    socket.send(data);
  }

  function disconnect() {
    socket.close();
    socket = null;
  }

  function uid() {
    if (localStorage.uid) {
      return localStorage.uid
    }
    uid = ""
    for (var i = 0; i < 32; i++) {
      // pick a char from [33;126]
      v = 33 + Math.round(Math.random() * 93.5);
      uid += String.fromCharCode(v);
    }
    localStorage.uid = uid
    return uid
  }

  return {
    connect : connect,
    disconnect : disconnect,
    send : send,
    setOnmessage : setOnmessage,
    uid : uid
  };
})();
