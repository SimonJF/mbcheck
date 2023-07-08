# MBC programs are not allowed to send their own name to themselves.
# (i.e., a ! m[a] is disallowed).
# This program demonstrates unsoundness caused by using messages to
# introduce aliasing in guard blocks.

# In the original MB calculus, this can be ruled out using dependency
# graphs: introduce a dependency between the mailbox handle and its
# payload, and introduce a dependency between the guarded MB and its
# continuations.

interface Carrier { CarrierMessage(Payload!) }
interface Payload { PayloadMessage(Payload!) }

# Receives a PayloadMessage from the mailbox, forward the name to the
# mailbox. At runtime, x and mb will be the same.
def recvAndFree(mb: Payload?) : Unit {
  guard mb : (PayloadMessage . *PayloadMessage) {
      receive PayloadMessage(x) from mb ->
          mb ! PayloadMessage(x);
          recvAndFree(mb)
  }
}

# Receives a name from the carrier, sends it along the payload handle.
# Note that at runtime, payload and payload2 will be the same.
def go(carrier: Carrier?, payload: Payload!) : Unit {
    guard carrier : (CarrierMessage) {
        receive CarrierMessage(payload2) from y ->
            payload ! PayloadMessage(payload2);
            free(y)
    }
}

# Sets everything in motion.
def main(): Unit {
  let carrier = new[Carrier] in
  let payload = new[Payload] in
  carrier ! CarrierMessage(payload);
  spawn { go(carrier, payload) };
  recvAndFree(payload)
}

(main() : Unit)
