interface Test { Foo(), Bar() }

def foo(x: Test?): Unit {
    guard x : Foo {
        receive Foo() from x -> free(x); ()
        receive Bar() from x -> free(x); ()
    }
}
