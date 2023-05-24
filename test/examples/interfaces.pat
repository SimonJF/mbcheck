interface Reply1 { Go() }
interface Reply2 { Go() }
interface Receiver { Ready1(Reply1!), Ready2(Reply2!) }

def go(self: Receiver?): Unit {
    guard self : Ready1 . Ready2 {
        receive Ready1(reply1) from mb ->
            guard mb : Ready2 {
                receive Ready2(reply2) from mb ->
                    reply1 ! Go();
                    reply2 ! Go();
                    free(mb)
            }
    }
}

def main(): Unit {
    let mb = new[Receiver] in
    spawn { go(mb) };
    let client1 = new[Reply1] in
    let client2 = new[Reply2] in
    mb ! Ready1(client1);
    mb ! Ready2(client2);
    guard client1 : Go {
        receive Go() from client1 -> free(client1)
    };
    guard client2 : Go {
        receive Go() from client2 -> free(client2)
    }
}

main()
