### Adapted from Savina/logmap
###
### Calculates the recurrence relation x_{n + 1} = r * x_n * (1 - x_n), where
### x is the term and r is the rate.

interface MasterMb {
  Start(),
  Result(Int)
}

interface WorkerMb {
  NextTerm(),
  GetTerm(),
  ResultWorker(Int),
  Stop()
}

interface TermMb {
  Done(Int)
}

interface ComputerMb {
  Compute(TermMb!, Int),
  StopCompute()
}

## Master process handling the creation of the worker and computer processes,
## in addition to launching the main loop.
def master(self: MasterMb?, startRate: Int, increment: Int): Unit {

  let computerMb1 = new [ComputerMb] in
  let rate1 = startRate + (1 * increment) in 
  spawn { computer(computerMb1, rate1) };

  let workerMb1 = new [WorkerMb] in
  let startTerm1 = 1 * increment in
  spawn { worker(workerMb1, 1, self, computerMb1, startTerm1) };

  let computerMb2 = new [ComputerMb] in
  let rate2 = startRate + (2 * increment) in 
  spawn { computer(computerMb2, rate2) };

  let workerMb2 = new [WorkerMb] in
  let startTerm2 = 2 * increment in
  spawn { worker(workerMb2, 2, self, computerMb2, startTerm2) };

  guard self: Start . (*Result) {
    receive Start() from self ->

      # We should have a loop around this line to send multiple NextTerm
      # messages, according to the number of terms we send to computer. For
      # now, let this be 2. Later we can refactor it.
      workerMb1 ! NextTerm();
      workerMb1 ! NextTerm();
      workerMb2 ! NextTerm();
      workerMb2 ! NextTerm();
      workerMb2 ! NextTerm();

      # Get result from worker as soon as finished. We should have many workers.
      # The number of workers is numWorkers and to each one, we send just one
      # request.
      workerMb1 ! GetTerm();
      workerMb2 ! GetTerm();

      # Collect results.
      master_loop(self, 0, workerMb1, computerMb1, workerMb2, computerMb2)
  }
}

## Master process main loop issuing term computation requests.
def master_loop(self: MasterMb?, termSum: Int, workerMb1: WorkerMb!, computerMb1: ComputerMb!, workerMb2: WorkerMb!, computerMb2: ComputerMb!): Unit {
  guard self: *Result {
    free -> 
      # We need not track whether the number of requests sent and replies 
      # received tallies. This is done implicitly by the type checker.
      
      # Notify workers and computers.
      workerMb1 ! Stop();
      workerMb2 ! Stop();
      computerMb1 ! StopCompute();
      computerMb2 ! StopCompute();
      
      # Print result.
      print(concat("Result is: ", intToString(termSum)))
    receive Result(term) from self ->

      # Accumulate computed term.
      master_loop(self, termSum + term, workerMb1, computerMb1, workerMb2, computerMb2)
  }
}

## Worker process handling term computation requests, delegating them to 
## computer processes.
def worker(self: WorkerMb?, id: Int, masterMb: MasterMb!, computerMb: ComputerMb!, currTerm: Int): Unit {
  guard self: (*NextTerm) . GetTerm . Stop {
    receive NextTerm() from self ->
            
      # Delegate computation of term to computer process via the local mailbox
      # termMb.
      let termMb = new [TermMb] in
      computerMb ! Compute(termMb, currTerm);
      guard termMb: Done {
        receive Done(term) from termMb ->
          free(termMb);
          worker(self, id, masterMb, computerMb, term)
      }
      

    receive GetTerm() from self ->
      masterMb ! Result(currTerm);
      guard self: (*NextTerm) . Stop {
        receive Stop() from self ->
          worker_exit(self)
      }
  }
}

## Worker process exit procedure flushing potential residual messages.
def worker_exit(self: WorkerMb?): Unit {
  guard self: *NextTerm {
    free -> ()
    receive NextTerm() from self ->
      worker_exit(self)
  }
}

## Computer process handling computation requests delegated by an associated
## worker process.
def computer(self: ComputerMb?, rate: Int): Unit {
  guard self: (*Compute) . StopCompute {
    free -> ()
    receive Compute(termMb, term) from self ->
      
      # Compute next term.
      termMb ! Done(rate * term * (1 - term));
      computer(self, rate)
    receive StopCompute() from self ->
      computer_exit(self)
  }
}

## Computer process exit procedure flushing potential residual messages.
def computer_exit(self: ComputerMb?): Unit {
  guard self: *Compute {
    free -> ()
    receive Compute(termMb, term) from self ->

      # Send back the same term value so that the final computation on the 
      # worker is kept fixed.
      termMb ! Done(term);
      computer_exit(self)
  }
}

## Launcher.
def main(): Unit {
  
  let masterMb = new [MasterMb] in
  spawn { master(masterMb, 3, 1) };

  masterMb ! Start()
}

main()
