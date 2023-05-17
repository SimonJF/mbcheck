# Shows the necessity of the explicit check that an annotation is a subtype of
# the inferred pattern in TC-Guard

interface Test { Foo(), Bar(), Baz() }

def foo(x: Test?): Unit {
    guard x : Foo  {
        receive Foo() from x -> free(x); ()
        receive Bar() from x ->
            guard x : Baz {
                receive Baz() from x -> free(x)
            }
    }
}
