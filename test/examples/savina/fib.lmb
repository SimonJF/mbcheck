### Adapted from Savina/fib
###
### Server that calculates the Fibonacci number sent in client requests.

interface FibMb {
  Req(FibMb!, Int),
  Resp(Int)
}

## Fibonacci process computing the (n - 1)st and (n - 2)nd terms.
def fib(self: FibMb?): Unit {
  guard self: Req {
    receive Req(replyTo, n) from self ->
      let term =
      if (n <= 2) {

        # Base case.
        free(self);
        1
      }
      else {

        # Inductive case (n - 1) and (n - 2). Delegate computation of (n - 1)st
        # and (n - 2)nd term to fib process replicas.
        let fib1Mb = new [FibMb] in
        spawn { fib(fib1Mb) };

        let fib2Mb = new [FibMb] in
        spawn { fib(fib2Mb) };

        fib1Mb ! Req(self, n - 1);
        fib2Mb ! Req(self, n - 2);

        # Combine results computed for the (n - 1)st and (n - 2)nd terms.
        guard self: Resp . Resp {
          receive Resp(f1) from self ->
            guard self: Resp {
              receive Resp(f2) from self ->
                free(self);
                f1 + f2
            }
          }
      } in

      replyTo ! Resp(term)
  }
}

## Launcher.
def main(): Unit {

  let fibMb = new [FibMb] in
  spawn { fib(fibMb) };

  let self = new [FibMb] in
  fibMb ! Req(self, 5);
  guard self: Resp {
    receive Resp(f) from self ->
      free(self);
      print(concat("Result: ", intToString(f)))
  }
}

main()
