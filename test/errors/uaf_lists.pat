interface Test { M(Test!1) }

def drain(x: Test?): Unit {
    guard x : M* {
        free -> ()
        receive M(y) from z -> drain(z)
    }
}

def go(): Unit {
    let x = new[Test] in
    let xs = (x cons (nil: [Test![U]])) in
    caseL xs : [Test!] of {
          nil -> ()
        | (a cons as) -> x ! M(a)
    };
    drain(x)
}

go()
