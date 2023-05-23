interface UAF { Message(Unit) }

def go(x : UAF?) : Unit {
    let a = x in
    guard x : (1 + *Message) {
        free -> a ! Message(())
        receive Message(z) from y ->
            a ! Message(());
            go(y)
    }
}


def main(): Unit {
  let x = new[UAF] in
  x ! Message(());
  go(x)
}

(main() : Unit)
