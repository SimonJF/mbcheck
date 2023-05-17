### Adapted from Savina/philosopher
###
### Dining philosophers problem.

interface PhilosopherMb {
  Start(),
  Denied(),
  Eat()
}

interface ArbiterMb {
  Hungry(PhilosopherMb!, Int),
  Done(Int),
  Exit()
}

## Philosopher process handling the launching of main loop.
def philosopher(self: PhilosopherMb?, id: Int, numRounds: Int, arbiterMb: ArbiterMb!): Unit {
  guard self: Start {
    receive Start() from self ->
      philosopher_loop(self, id, numRounds, arbiterMb)
  }
}

## Philosopher process main loop issuing hunger requests.
def philosopher_loop(self: PhilosopherMb?, id: Int, numRounds: Int, arbiterMb: ArbiterMb!): Unit {
  
  arbiterMb ! Hungry(self, id);

  guard self: Denied + Eat + 1 {
    free -> ()
    receive Denied() from self ->
      philosopher_loop(self, id, numRounds, arbiterMb)
    receive Eat() from self ->
      arbiterMb ! Done(id);

      if (numRounds <= 0) {
        arbiterMb ! Exit();
        free(self)
      }
      else {
        philosopher_loop(self, id, numRounds - 1, arbiterMb)
      }
  }
}

## Arbiter process managing the allocation and deallocation of forks to 
## philosopher processes, as well as coordinating their termination.
def arbiter(self: ArbiterMb?, numExitedPhilosophers: Int, fork1: Bool, fork2: Bool): Unit {
  guard self: *(Hungry + Done + Exit) {
    free -> 
      ()
    receive Hungry(philosopherMb, philosopherId) from self ->

      if (forks_available(philosopherId, fork1, fork2)) {
        
        # Notify philosopher and update fork allocation for specified 
        # philosopher ID.
        philosopherMb ! Eat();
        allocate_forks(philosopherId, fork1, fork2);
        arbiter(self, numExitedPhilosophers, fork1, fork2)
      }
      else {

        # One or both forks occupied.
        philosopherMb ! Denied();
        arbiter(self, numExitedPhilosophers, fork1, fork2)
      }

    receive Done(philosopherId) from self ->
      
      # Reset fork allocation.
      deallocate_forks(philosopherId, fork1, fork2);
      arbiter(self, numExitedPhilosophers, fork1, fork2)
    
    receive Exit() from self ->

      if (numExitedPhilosophers <= 0) {
        arbiter_exit(self)
      }
      else {
        arbiter(self, numExitedPhilosophers - 1, fork1, fork2) 
      }
  }
}

## Stub. Checks whether the forks for the specified philosopher ID are 
## available.
def forks_available(id: Int, fork1: Bool, fork2: Bool): Bool {
  true
}

## Stub. Toggles the Boolean values of the fork variables to indicate that they
## are in use by the philosopher with the specified ID.
def allocate_forks(id: Int, fork1: Bool, fork2: Bool): Unit {
  ()
}

## Stub. Toggles the Boolean values of the fork variables to indicate that they
## are relinquished by the philosopher with the specified ID.
def deallocate_forks(id: Int, fork1: Bool, fork2: Bool): Unit {
  ()
}

## Arbiter process exit procedure flushing potential residual messages.
def arbiter_exit(self: ArbiterMb?): Unit {
  guard self: *(Hungry + Done + Exit) {
    free -> ()
    receive Hungry(philosopherMb, philosopherId) from self ->
      arbiter_exit(self)
    receive Done(id) from self ->
      arbiter_exit(self)
    receive Exit() from self ->
      arbiter_exit(self)
  }
}

## Launcher.
def main(numRounds: Int): Unit {

  let arbiterMb = new [ArbiterMb] in
  spawn { arbiter(arbiterMb, 2, false, false) };

  let philosopherMb1 = new [PhilosopherMb] in
  spawn { philosopher(philosopherMb1, 0, numRounds, arbiterMb) };
  philosopherMb1 ! Start();

  let philosopherMb2 = new [PhilosopherMb] in
  spawn { philosopher(philosopherMb2, 0, numRounds, arbiterMb) };
  philosopherMb2 ! Start()
}

main(5)
