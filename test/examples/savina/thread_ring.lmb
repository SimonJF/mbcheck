### Adapted from Savina/threadring
###
### A ring of actors modelling a cyclic workflow with a chain of tasks, where
### each actor decrements a token and forwards it until the token value reaches
### zero.

interface ActorMb {
  Data(ActorMb!),
  Ping(Int),
  Exit(Int)
}

## Actor process handling the launching of main loop.
## numActors: Number of actors in ring
def actor(self: ActorMb?, numActors: Int): Unit {
  guard self: Data . (*Ping) . (*Exit) {
    receive Data(neighborMb) from self ->
      actor_loop(self, numActors, neighborMb)
  }
}

## Ping process main loop issuing pings and exits. 
def actor_loop(self: ActorMb?, numActors: Int, neighborMb: ActorMb!): Unit {
  
  guard self: (*Ping) . (*Exit) {
    # Required because of the if condition in the receive statement for Ping 
    # where the type checker does not have enough info to determine whether
    # the if part is ever taken (i.e., it does not evaluate the condition).
    free -> ()
    receive Ping(pingsLeft) from self ->
      
      if (pingsLeft <= 0) {
        neighborMb ! Exit(numActors);
        actor_exit(self)

      } 
      else {
        neighborMb ! Ping(pingsLeft - 1);
        actor_loop(self, numActors, neighborMb)  
      }
    receive Exit(exitsLeft) from self ->
      if (exitsLeft <= 0) {
        ()
      }
      else{
        neighborMb ! Ping(exitsLeft - 1)
      };
      actor_exit(self)
  }
}

## Actor process exit procedure that flushes potential residual messages.
def actor_exit(self: ActorMb?): Unit {
  guard self: *Ping . (*Exit) {
    free -> ()
    receive Ping(pingsLeft) from self ->
      actor_exit(self)
    receive Exit(exitsLeft) from self ->
      actor_exit(self)
  }
}

## Initializes ring of actors. The number of participants in the ring is 
## parametrized by 'numActors'. 
def init_ring(numActors: Int, mainMb: ActorMb!): Unit {

  if (numActors < 2) {
    # Cannot have a ring with less than two actors.
    ()
  }
  else {

    # Create first mailbox and spawn corresponding actor..
    let firstActorMb = new [ActorMb] in
    spawn { actor(firstActorMb, numActors) };

    # Create list of actors and close loop.
    let tailActorMb = create_actors(numActors - 2, numActors, firstActorMb) in
    tailActorMb ! Data(firstActorMb);
    
    # Notify main process of first actor mailbox.
    mainMb ! Data(firstActorMb)
  }
}

## Creates a series of actors, linking each actor to the one preceding it by
## sending the address of its mailbox to the previous actor.
def create_actors(count: Int, numActors: Int, prevActorMb: ActorMb!): ActorMb![R] {

  let actorMb = new [ActorMb] in
  spawn { actor(actorMb, numActors) };

  # Link current actor to previous one.
  prevActorMb ! Data(actorMb);

  # Create next actor.
  if (count < 0) {

    # All actors created.
    actorMb
  }
  else {
    create_actors(count - 1, numActors, actorMb)
  }
}

## Launcher.
## numActors : num actors
## numRounds: rounds (i.e. number of messages).
def main(numActors: Int, numRounds: Int): Unit {

  let mainMb = new [ActorMb] in
  init_ring(numActors, mainMb);

  guard mainMb: Data + 1 {
    free -> ()
    receive Data(firstActorMb) from mainMb ->
      firstActorMb ! Ping(numRounds);
      free(mainMb)
  }
}

main(5, 1000)