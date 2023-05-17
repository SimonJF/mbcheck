# Shows the necessity of the explicit check that an annotation is a subtype of
# the inferred pattern in TC-Guard

interface Test { Foo(), Bar() }

def foo(x: Test?): Unit {
    guard x : Foo {
        free -> ()
        receive Foo() from x -> free(x)
    }
}
