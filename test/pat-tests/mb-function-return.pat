interface Test { }

def main(): Unit {
    let f = fun(x: Test?): Test? { x } in
    free(f(new[Test]))
}

main()
