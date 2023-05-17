### Adapted from Savina/pingpong
###
### Two actors that repeatedly send and reply a Ping message back and forth.
### A ping count is maintained, which, once exhausted, induces the system to 
### terminate.

interface PingMb {
  Start(PongMb!),
  Pong(PongMb!)
}

interface PongMb {
  Ping(PingMb!),
  Stop()
}

## Ping process handling launching of main loop.
def ping(self: PingMb?, pingsLeft: Int): Unit {
  
  guard self: Start {
    receive Start(pongMb) from self ->
      send_ping(self, pongMb, pingsLeft)
  }
}

## Issues a ping to the ponger process and loops or exits if no pings left.
def send_ping(self: PingMb?, pongMb: PongMb!, pingsLeft: Int): Unit {
  if (pingsLeft > 0) {
    pongMb ! Ping(self);
    ping_loop(self, pingsLeft - 1)
  }
  else {
    pongMb ! Stop();
    free(self)
  }
}

## Ping process main loop issuing ping requests.
def ping_loop(self: PingMb?, pingsLeft: Int): Unit {

  guard self: Pong {
    receive Pong(pongMb) from self ->
      send_ping(self, pongMb, pingsLeft)
  }
}

## Pong process loop issuing pong replies.
def pong(self: PongMb?): Unit {
  
  # guard self: *(Ping + Stop) {
  guard self: Ping + Stop {
    # free -> ()
    receive Ping(pingMb) from self ->
      pingMb ! Pong(self);
      pong(self)
    receive Stop() from self ->
      free(self)
  }
}

## Launcher.
def main(): Unit {

  let pongMb = new [PongMb] in
  spawn { pong(pongMb) };

  let pingMb = new [PingMb] in
  spawn { ping(pingMb, 5) };

  pingMb ! Start(pongMb)
}

main()
