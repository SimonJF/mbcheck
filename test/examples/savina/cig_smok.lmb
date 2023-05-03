### Adapted from Savina/cigsmok.
###
### A benchmark modelling n smokers and one arbiter that decides which smoker to
### allow to smoke. The benchmark is parameterized by the number of smoker 
### processes. Since the array type is not available in Pat, we fix the number 
### of account processes to 3.

interface ArbiterMb {
  Start(),
  StartedSmoking()
}

interface SmokerMb {
  StartSmoking(Int),
  Exit()
}

## Arbiter process handling the creation of smokers and launching of main loop.
def arbiter(self: ArbiterMb?, numRounds: Int): Unit {

  let smokerMb1 = new [SmokerMb] in
  spawn { smoker(smokerMb1, self) };

  let smokerMb2 = new [SmokerMb] in
  spawn { smoker(smokerMb2, self) };

  let smokerMb3 = new [SmokerMb] in
  spawn { smoker(smokerMb3, self) };

  guard self: Start . (*StartedSmoking) {
    receive Start() from self ->
      
      notify_smoker(smokerMb1, smokerMb2, smokerMb3);
      arbiter_loop(self, numRounds, smokerMb1, smokerMb2, smokerMb3)
  }
}

## Randomly chooses the smoker and requests it to smoke.
def notify_smoker(smokerMb1: SmokerMb!, smokerMb2: SmokerMb!, smokerMb3: SmokerMb!): Unit {

  let smokerId = rand(2) in
  let sleepTimeMs = 1000 in

  if (smokerId == 0) {
    smokerMb1 ! StartSmoking(rand(sleepTimeMs))
  }
  else {
    if (smokerId == 1) {
      smokerMb2 ! StartSmoking(rand(sleepTimeMs))
    }
    else {
      smokerMb3 ! StartSmoking(rand(sleepTimeMs))
    }
  }
}

## Notifies all smokers to terminate.
def notify_smoker_exit(smokerMb1: SmokerMb!, smokerMb2: SmokerMb!, smokerMb3: SmokerMb!): Unit {
  smokerMb1 ! Exit();
  smokerMb2 ! Exit();
  smokerMb3 ! Exit()
}

## Arbiter process main loop issuing start smoking requests and handling started 
## smoking replies.
def arbiter_loop(self: ArbiterMb?, numRounds:Int, smokerMb1: SmokerMb!, smokerMb2: SmokerMb!, smokerMb3: SmokerMb!): Unit {
  guard self: *StartedSmoking {
    free -> 
      ()
    receive StartedSmoking() from self ->
      
      # The if here introduces the internal choice, which means that on the 
      # receiver side I might or might not receive the message. In this case,
      # the smoker might or might nor receive the Exit message, and must either
      # use (Exit + 1) or (*Exit) in its pattern.
      
      if (numRounds <= 0) {
        notify_smoker_exit(smokerMb1, smokerMb2, smokerMb3)
      }
      else {
        notify_smoker(smokerMb1, smokerMb2, smokerMb3)
      };

      # Arbiter needs to service all requests before, even if it has sent the 
      # Exit messages to smokers. Remember that smokers may still be processing
      # StartSmoking messages, but the arbiter has already issued Exit messages
      # and it still needs to await all the StartedSmoking replies before 
      # terminating. This is why we do not have an exit_arbiter flush function. 
      arbiter_loop(self, numRounds - 1, smokerMb1, smokerMb2, smokerMb3)
  }
}

## Smoker process main loop handling start smoking requests and issuing started
## smoking replies to/from the arbiter.
def smoker(self: SmokerMb?, arbiterMb: ArbiterMb!): Unit {
  # Smoker may be asked to smoke more than once, or none. This is why the *.
  guard self: (*StartSmoking) . (*Exit) {
    free -> 
      () # Since the smoker might not even receive an Exit/StartSmoking message due to the if condition above.
    receive StartSmoking(ms) from self ->
      arbiterMb ! StartedSmoking();
      sleep(ms);
      smoker(self, arbiterMb)
    receive Exit() from self ->
      smoker_exit(self)
  }
}

## Smoker process exit procedure that flushes potential residual messages.
def smoker_exit(self: SmokerMb?): Unit {
  guard self: (*StartSmoking) . (*Exit) {
    free -> () # In case I have one or more Exit/StartSmoking messages due to the if condition above.
    receive StartSmoking(ms) from self ->
      smoker_exit(self)
    receive Exit() from self ->
      smoker_exit(self)
  }
}

## Launcher.
def main(): Unit {
  
  let arbiterMb = new [ArbiterMb] in
  spawn { arbiter(arbiterMb, 10) };

  arbiterMb ! Start()
}

main()
