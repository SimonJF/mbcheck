interface Dummy { }
def main(): Unit {
    let mb = new[Dummy] in
    let f = linfun(x: Dummy?): Unit { free(x) } in
    ()
}
