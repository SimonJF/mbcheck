# Sessions example from "Mailbox Types for Unordered Interactions"
interface Alice {
    AReply1(Arbiter!),
    AReply2(Arbiter!),
    AReply3(Int)
}

interface Carol {
    CReply1(Int, Arbiter!),
    CReply2(Int, Arbiter!),
    CReply3()
}

interface Arbiter {
    ASend1(Int, Alice!),
    ASend2(Int, Alice!),
    AReceive(Alice!),
    CReceive1(Carol!),
    CReceive2(Carol!),
    CSend(Int, Carol!)
}


def alice(self: Alice?, arb: Arbiter!): Unit {
    arb ! ASend1(4, self);
    let self =
        guard self : AReply1 {
            receive AReply1(arb) from self ->
                arb ! ASend2(2, self);
                guard self : AReply2 {
                        receive AReply2(arb) from self ->
                            arb ! AReceive(self);
                            self
                }
        }
    in
    guard self : AReply3 {
        receive AReply3(res) from self ->
            print(intToString(res));
            free(self)
    }
}

def carol(self: Carol?, arb: Arbiter!): Unit {
    arb ! CReceive1(self);
    let self =
        guard self : CReply1 {
            receive CReply1(x, arb) from self ->
                arb ! CReceive2(self);
                guard self : CReply2 {
                    receive CReply2(y, arb) from self ->
                        arb ! CSend(x + y, self);
                        self
                }
        }
    in
    guard self : CReply3 {
        receive CReply3() from self -> free(self)
    }
}

def arbiter(self: Arbiter?): Unit {
    let self =
        guard self : ASend1 . CReceive1 {
            receive ASend1(x, aliceMB) from self ->
                guard self : CReceive1 {
                    receive CReceive1(carolMB) from self ->
                        aliceMB ! AReply1(self);
                        carolMB ! CReply1(x, self);
                        self
                }
        }
    in
    let self =
        guard self : ASend2 . CReceive2 {
            receive ASend2(y, aliceMB) from self ->
                guard self : CReceive2 {
                    receive CReceive2(carolMB) from self ->
                        aliceMB ! AReply2(self);
                        carolMB ! CReply2(y, self);
                        self
                }
        }
    in
    guard self : CSend . AReceive {
        receive CSend(res, carolMB) from self ->
            guard self : AReceive {
                receive AReceive(aliceMB) from self ->
                    aliceMB ! AReply3(res);
                    carolMB ! CReply3();
                    free(self)
            }
    }
}

def main(): Unit {
    let aliceMB = new[Alice] in
    let carolMB = new[Carol] in
    let arbiterMB = new[Arbiter] in
    spawn { alice(aliceMB, arbiterMB) };
    spawn { carol(carolMB, arbiterMB) };
    arbiter(arbiterMB)
}

main()

