// this keeps the shiny app alive for apps hosted on systems with keep alive intervals
// every dt for the dt value set by the system, shinyapp.js "disconnects" the app from the server
// this displays the gray screen overlay
// If an app is disconnect, Shiny.shinyapp.reconnect() can reconnect it
// Shiny.shinyapp.$socket does send a message over the socket using Shiny.shinyapp.$socket.send()

// m - number of minutes that you want to keep the Shiny App connected over the socket
// ms - milliseconds of keep alive interval of ALB websocket connection (60 seconds)
function keepAlive(m, ms = 1000*50) {
  var x = []
  for (let i=1; i<m; i++) {
    x.push(i)
  }
  var t = setTimeouts(x, ms);
  // mouse movements are not working
  window.onmousemove = resetTimer; // mouse movement
  window.onmousedown = resetTimer; // mouse movement
  window.onclick = resetTimer; // button click
  window.onscroll = resetTimer; // scroll
  window.onkeypress = resetTimer; // keyboard
  // manually close connection - can be appending to t0 in setTimeouts
  function disconnect() {
    Shiny.shinyapp.$socket.close()
  }
  function resetTimer() {
    clearTimeouts(t);
    t = setTimesouts(x, ms);
  }
  function setTimeouts(x, ms) {
    let t0 = []
    for (let i of x) {
      // account for web traffic latency by subtracting 60 ms
      t0.push(setTimeout(sendMessage, i*ms-60, i))
    }
    return(t0)
  }
  function clearTimeouts(t) {
    for (i of t) {
      clearTimeout(i);
    }
  }
  function sendMessage(i) {
    // Shiny.shinyapp.sendInput()
    Shiny.setInputValue("keep_alive", i);
  }
}
Shiny.addCustomMessageHandler('keep-alive', function(m){console.log(m)});
keepAlive(m=60);
