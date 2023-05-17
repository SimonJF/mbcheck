interface Barrier { Reply() }

interface Account {
    Debit(Int, Barrier!),
    Credit(Int, Account!, Barrier!),
    Stop()
}

def notify(barrier: Barrier!): Unit {
    barrier ! Reply()
}

def await(barrier: Barrier?): Unit {
    guard barrier : Reply {
        receive Reply() from barrier ->
            free(barrier)
    }
}

def account(self: Account?, balance: Int): Unit {
    guard self : ((*Debit) . (*Credit))  {
        # receive Stop() from self -> free(self)
        free -> ()

        receive Debit(amount, ack) from self ->
            notify(ack);
            account(self, balance - amount)

        receive Credit(amount, payer, ack) from self ->
            # Create new barrier for this transaction
            let barrier = new[Barrier] in
            # Debit the payer
            payer ! Debit(amount, barrier);
            # Wait for confirmation
            await(barrier);
            # Notify initiator of this transaction
            notify(ack);
            account(self, balance + amount)
    }
}

def main(): Unit {
    # Alice
    let alice = new[Account] in
    spawn { account(alice, 10) };
    # Bob
    let bob = new[Account] in
    spawn { account(bob, 15) };
    ### Ack
    let barrier = new[Barrier] in
    ####
    alice ! Credit(10, bob, barrier);
    await(barrier)
}
