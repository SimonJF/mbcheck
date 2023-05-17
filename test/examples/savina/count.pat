### Adapted from Savina/count
###
### A generator actor sends messages to a receiving actor who increments a 
### counter upon receiving a message. The generator actor retrieves the total.

interface ProducerMb {
  Inc(),
  Total(Int)
}

interface CounterMb {
  Inc(),
  Get(ProducerMb!)
}

## Producer process handling the launching of main loop.
def producer(self: ProducerMb?, counterMb: CounterMb!, numMessages: Int): Unit {
  guard self: Inc {
    receive Inc() from self ->
      producer_loop(self, counterMb, numMessages)
  }
}

## Producer process main loop issuing increment requests.
def producer_loop(self: ProducerMb?, counterMb: CounterMb!, numMessages: Int): Unit {
  if (numMessages <= 0) {
    counterMb ! Get(self);
    producer_exit(self, numMessages)
  }
  else {
    counterMb ! Inc();
    producer_loop(self, counterMb, numMessages - 1)
  }
}

## Producer process exit procedure handling the final Total message.
def producer_exit(self: ProducerMb?, numMessages: Int): Unit {
  
  guard self: Total {
    receive Total(total) from self ->
      print(concat("Total: ", intToString(total)));
      free(self)
  }
}

## Counter process main loop handling increment requests.
def counter(self: CounterMb?, total: Int): Unit {
  guard self: (*Inc) . Get {
    free -> ()
    receive Inc() from self ->
      counter(self, total + 1)
    receive Get(producerMb) from self ->
      producerMb ! Total(total);
      
      # Function counter_exit/1 is required here, rather than the BIF free/1. 
      # This is because, in principle, additional Inc messages may be still 
      # be enqueued in the mailbox. Remember that commutative regular 
      # expressions specify that (at the same moment), the mailbox contains any
      # number of Inc messages and exactly one Get message, without imposing
      # any order in which these are consumed. Even though it is clear from 
      # the logic of the branching statement in the function producer_loop/3, 
      # the type checker has no way of determining this. It is an approximative
      # analysis, after all (in particular, it does not evaluate the expression
      # in the if condition). At any rate, the best that can be done in this 
      # case is for the counter to flush from the mailbox potential residual Inc
      # messages, once the Get message is processed.
      counter_exit(self)
  }
}

## Counter process exit procedure that flushes potential residual messages.
def counter_exit(self: CounterMb?): Unit {
  guard self: *Inc {
    free -> ()
    receive Inc() from self ->
      counter_exit(self)
  }
}

## Launcher.
def main(): Unit {

  let counterMb = new [CounterMb] in
  spawn { counter(counterMb, 0) };

  let producerMb = new [ProducerMb] in
  spawn { producer(producerMb, counterMb, 5) };

  producerMb ! Inc()
}

main()
