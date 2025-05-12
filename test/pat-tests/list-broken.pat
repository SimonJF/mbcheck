interface Test {
  Ping()
}

def spawnMbs(acc : [Test!]) : [Test!] {
    acc
}

def test(mbs : [Test!]) : Unit {
    ()
}

def main() : Unit {
    let mbs = spawnMbs(nil : [Test!]) in
    test(mbs)
}
