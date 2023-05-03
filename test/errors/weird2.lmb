interface Weird { WeirdMsg(Unit) }

def go(x : Weird?) : Unit {
    spawn { x ! WeirdMsg(())};
    let a = x in
    guard x : (1 + *WeirdMsg) {
        free -> a ! WeirdMsg(())
        receive WeirdMsg(z) from y ->
            a ! WeirdMsg(());
            go(y)
    }
}


def main(): Unit {
  let x = new[Weird] in
  go(x)
}

(main() : Unit)
