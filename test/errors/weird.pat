interface Weird { WeirdMsg(Unit) }

def go(x : Weird?) : Unit {
    spawn { x ! WeirdMsg(())};
    guard x : (1 + *WeirdMsg) {
        free -> x ! WeirdMsg(())
        receive WeirdMsg(z) from y ->
            x ! WeirdMsg(());
            go(y)
    }
}


def main(): Unit {
  let x = new[Weird] in
  go(x)
}

(main() : Unit)
