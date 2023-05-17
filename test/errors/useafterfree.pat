interface Test { M(Int), N(String) }

def main(): Unit {
    let a = new[Test] in
    guard a : N {
        receive N(x) from a -> free(a)
    };
    a ! N("Hello")
}


(main() : Unit)
