interface Test { Arg(Int) }

def nested(mb: Test?): Unit {
    guard mb : Arg . Arg {
        receive Arg(x) from mb1 ->
            guard mb1 : Arg {
                receive Arg(y) from mb2 ->
                    free(mb2); print(intToString(x + y))
            }
    }
}

def pairs(mb: Test?): Unit {
    let (x, mb1) =
        guard mb : Arg . Arg {
            receive Arg(x) from mb1 -> (x, mb1)
        }
    in
    guard mb1 : Arg {
        receive Arg(y) from mb2 -> 
            free(mb2); print(intToString(x + y))
    }
}

def main(): Unit {
    let mb1 = new[Test] in
    mb1 ! Arg(1);
    mb1 ! Arg(2);
    spawn { nested(mb1) };

    let mb2 = new[Test] in
    mb2 ! Arg(1);
    mb2 ! Arg(2);
    spawn { pairs(mb2) }
}
main()
