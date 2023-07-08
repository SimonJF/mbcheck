interface UAF { Message(Unit) }

def go(x : UAF?) : Unit {
    guard x : (1 + *Message) {
        free -> x ! Message(())
        receive Message(z) from y ->
            x ! Message(());
            go(y)
    }
}


def main(): Unit {
  let x = new[UAF] in
    spawn { x ! Message(())};
  go(x)
}

(main() : Unit)
