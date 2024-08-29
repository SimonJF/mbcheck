def main(): ((Bool * Int) * String) {
    let (x, z) = ((1, "hello"), true) in
    let (x1, x2) = x in
    ((z, x1), x2)
}
main()
