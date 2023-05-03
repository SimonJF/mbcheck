interface Carrier { CarrierMessage(Payload!) }
interface Payload { PayloadMessage(Unit) }

def freePayload(mb: Payload?): Unit {
    guard mb: (PayloadMessage.PayloadMessage) {
        receive PayloadMessage(x) from mb ->
            guard mb : (PayloadMessage) {
                receive PayloadMessage(y) from mb ->
                    free(mb)
            }
    }
}

# Another trick to introduce aliasing.
# What happens is we create a carrier and payload,
# and send the payload along the carrier.
#
# We then receive from the carrier, free the carrier,
# return the received endpoint, and let-bind it.
#
# By returning a send endpoint, we can alias it. The
# usual trick of requiring disjoint environments in
# a `let` doesn't work because the name is not contained
# in the `guard` environment; rather, it is introduced
# by the `receive` expression.
def main(): Unit {
  let carrier = new[Carrier] in
  let payload = new[Payload] in


  spawn { carrier ! CarrierMessage(payload) };
  spawn { freePayload(payload) };

  let x = guard carrier : CarrierMessage {
      receive CarrierMessage(x) from carrier ->
        free(carrier); x
  } in

  x ! PayloadMessage(());
  payload ! PayloadMessage(())
}

(main() : Unit)
