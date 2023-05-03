
# Mailbox message interfaces.
interface AccountMb {
    Debit(Int, FutureMb!),
    Credit(Int, AccountMb!, FutureMb!),
    Stop()
}

interface FutureMb {
    Reply()
}

# Issues a debit request to the recipient account with the specified amount.
#
# amount: Amount to debit.
# recipient: Mailbox where the debit request is deposited.
#
# Returns: Mailbox handle.
def await(amount: Int, recipient: AccountMb!): FutureMb? {
    let future = new [FutureMb] in
    spawn { recipient ! Debit(amount, future) };
    future
}

# Blocks until an acknowledgement is deposited in the specified mailbox.
#
# future: Mailbox where the acknowledgement from the recipient account is 
#         deposited.
#
# An account can be terminated at any moment by other processes. In such cases,
# it is possible for pending debit requests not to be acknowledged. The function
# caters for this possibility and outputs a warning, but does not block the 
# caller.
#
# Returns: Unit value. 
def resume(future: FutureMb?): Unit {
    # Reply + 1 handles the case when the Debit request issued by the function 
    # await/2 remains without a Reply when another account issues a Stop request 
    # to the same account the Debit was directed, and the account shuts down, 
    # leaving the Debit request unacknowledged.
    guard future: Reply + 1 {
        free ->
            print("WARN: Did not receive Reply ack from account!")
        receive Reply() from future ->
            free(future)
    }
}

# Empties the specified account mailbox of stale messages.
#
# account: Account mailbox to flush.
# stale: Count of stale messages flushed.
#
# Returns: Count of stale messages. 
def flush(account: AccountMb?, stale: Int): Int {
    guard account: (*Debit) . (*Credit) {
        free -> stale
        receive Debit(amount, sender) from account ->
            flush(account, stale + 1)
        receive Credit(amount, recipient, sender) from account ->
            flush(account, stale + 1)
    }
}

# Account server loop handling incoming instructions.
#
# self: Mailbox where account instructions are deposited.
# balance: Account balance. Permitted to be +ve or -ve.
#
# Returns: Unit value.
def account(self: AccountMb?, balance: Int): Unit {
    guard self: ((*Debit) . (*Credit)) . Stop {
        receive Debit(amount, sender) from self ->
            sender ! Reply();
            account(self, balance + amount)
        receive Credit(amount, recipient, sender) from self ->

            # Issue blocking Debit request and wait for reply.
            let future = await(amount, recipient) in
            resume(future);

            # Communicate to sender that Credit instruction was successful and
            # update account accordingly.
            sender ! Reply();
            account(self, balance - amount)
        receive Stop() from self ->
            print("INFO: Terminating account.");
            
            # Terminating the server potentially leaves queued Debit and Credit
            # instructions in mailbox. Flush empty mailbox and print count of
            # stale messages, if any.
            let stale = flush(self, 0) in
                if (stale > 0) {
                    print("WARN: Flushed ");
                    print(intToString(stale));
                    print(" message(s)!")
                }
            else { 
                () 
            }
    }
}

# Launcher.
def main(): Unit {
    
    # Create Alice's and Bob's accounts.
    let alice = new [AccountMb] in
    spawn { account(alice, 5) };

    let bob = new [AccountMb] in
    spawn { account(bob, 20) };

    # Instruct Bob to credit alice.
    let self = new [FutureMb] in
    bob ! Credit(20, alice, self);

    # Receive acknowledgement (if any, see resume/1 for details) for Credit 
    # transaction.
    resume(self);

    # Stop Alice's and Bob's accounts.
    alice ! Stop();
    bob ! Stop()
}

main()
