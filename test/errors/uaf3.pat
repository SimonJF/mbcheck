interface UAF { Message(Unit) }

def go(x : UAF?) : Unit {
    let a : Unit = 
        guard x : *Message {
            free -> x ! Message(())
            receive Message(z) from y ->
                y ! Message(());
                go(y)
        }
    in
    x ! Message(())
}


def main(): Unit {
  let x = new[UAF] in
  go(x)
}

(main() : Unit)
