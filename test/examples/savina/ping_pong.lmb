### Adapted from Savina/pingpong
###
### Two actors that repeatedly send and reply a Ping message back and forth.
### A ping count is maintained, which, once exhausted, induces the system to 
### terminate.

interface PingMb {
  Start(),
  Pong()
}

interface PongMb {
  Ping(PingMb!),
  Stop()
}

## Ping process handling the launching of main loop.
def ping(self: PingMb?, pongMb: PongMb!, pingsLeft: Int): Unit {
  
  guard self: Start {
    receive Start() from self ->
      ping_loop(self, pongMb, pingsLeft)
  }
}

## Ping process main loop issuing ping requests.
def ping_loop(self: PingMb?, pongMb: PongMb!, pingsLeft: Int): Unit {

  if (pingsLeft > 0) {

    # Issue ping and await reply. Reply 
    pongMb ! Ping(self);
    guard self: Pong + 1 {
      free -> 
        ()
      receive Pong() from self -> 
        ping_loop(self, pongMb, pingsLeft - 1)
    }
  }
  else {

    # No more pings to issue: notify ponger to stop.
    pongMb ! Stop();
    free(self) 
  }
}

## Pong process loop issuing pong replies.
def pong(self: PongMb?): Unit {
  
  guard self: *(Ping + Stop) {
    free -> ()
    receive Ping(pingMb) from self ->
      pingMb ! Pong();
      pong(self)
    receive Stop() from self ->
      pong_exit(self)
  }
}

## Pong process exit procedure that flushes potential residual messages.
def pong_exit(self: PongMb?): Unit {
  guard self: *(Ping + Stop) {
    free -> ()
    receive Ping(pingMb) from self ->
      pong_exit(self)
    receive Stop() from self ->
      pong_exit(self)
  }
}

## Launcher.
def main(): Unit {

  let pongMb = new [PongMb] in
  spawn { pong(pongMb) };

  let pingMb = new [PingMb] in
  spawn { ping(pingMb, pongMb, 5) };

  pingMb ! Start()
}

main()
