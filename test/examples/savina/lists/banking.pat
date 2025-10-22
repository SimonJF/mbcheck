### Adapted from Savina/banking.
###
### A benchmark that implements a request-reply chain passing scenario. Several
### account processes are spawned, each of which may be sent a credit request
### by the central teller process. The benchmark is parameterized by the number
### of account processes.

interface TellerMb {
  Start(),
  Reply()
}

interface AccountMb {
  Debit(AccountMb!, Int),
  Credit(TellerMb!, Int, AccountMb!),
  Done(),
  Stop()
}

def spawnAccounts(self: TellerMb?, numsAccounts: [Int], soFar: Int, acc: [AccountMb!]) : (TellerMb? * [AccountMb!]) {
  caseL numsAccounts : [Int] of {
    nil -> (self, acc)
    | (n cons ns) ->
        let accountMb = new [AccountMb] in
        spawn { account(accountMb, soFar, n)};
        spawnAccounts(self, ns, soFar + 1, (accountMb cons acc))
    }
  }

## Teller process handling the creation of account processes and launching of
## main loop.
def teller(self: TellerMb?, numAccounts : Int, numsAccounts: [Int]): Unit {

  let (self, accountMbs) = spawnAccounts(self, numsAccounts, 1, (nil : [AccountMb!])) in

  guard self: Start {
    receive Start() from self ->

      # Note. Even if the randomization of message sending is performed in a
      # different processes, there is no risk of a race condition where the
      # messages are sent to a non-existing mailbox. This is because, as can be
      # gleaned above, mailboxes are created in sequential fashion within this
      # (i.e., the teller) process.
      spawn { generate_work(self, numAccounts, accountMbs) } ;
      teller_loop(self, accountMbs)
  }
}

## Randomly chooses the source account.
def generate_work(tellerMb: TellerMb!, numAccounts: Int, accs : [AccountMb!]): Unit {

  # Randomly choose source account from which the funds shall be taken.
  let sourceId = rand(numAccounts - 1) in # -1 because rand() is 0-indexed.

  let (choice, rest) = pick(accs, sourceId, (nil : [AccountMb!])) in
  choose_dst_acc(tellerMb, numAccounts, choice, rest)
}

def pick(accs : [AccountMb!], index : Int, rest : [AccountMb!]) : ([AccountMb!] * [AccountMb!]) {
if (index == 0) {
    caseL accs : [AccountMb!] of {
        nil -> ((nil : [AccountMb!]), rest)
        | (a cons as) -> ((a cons (nil : [AccountMb!])), append(rest, as))
    }}
else {
    caseL accs : [AccountMb!] of {
        nil ->  ((nil : [AccountMb!]), rest)
        | (a cons as) -> pick(as, index - 1, (a cons rest))
    }}
}

def append(l1 : [AccountMb!], l2: [AccountMb!]) : [AccountMb!] {
    caseL l1 : [AccountMb!] of {
        nil -> l2
        | (a cons as) -> append(as, (a cons l2))
    }
}

## Randomly chooses the destination account and issues a credit request. The
## function ensures that the source and destination account are different.
def choose_dst_acc(tellerMb: TellerMb!, numAccounts: Int, srcAccountMb: [AccountMb![R]], dstAccountMbs : [AccountMb!]): Unit {

  # Randomly choose destination account to which funds shall be deposited. -2
  # because rand() is 0-indexed, and because we do not include the source
  # account in the random choice (i.e., the source account is not permitted to
  # send funds to itself).
  let dstAccountId = rand(numAccounts - 2) in

  let (dstAccount, rest) = (pick(dstAccountMbs, dstAccountId, (nil : [AccountMb!])) : ([AccountMb!Credit] * [AccountMb!1]))
  in

  let amount = rand(200) in
    caseL srcAccountMb : [AccountMb!] of {
    nil -> caseL dstAccount : [AccountMb!] of {
        nil -> ()
        | (d cons ds) -> ()
    }
    | (a cons as) -> caseL dstAccount : [AccountMb!] of {
        nil -> ()
        | (d cons ds) -> d ! Credit(tellerMb, amount, a)
    }
  }
}

## Teller process main loop handling replies from accounts.
def teller_loop(self: TellerMb?, accountMbs : [AccountMb!]): Unit {
  guard self: Reply* {
    free ->
      # All credit requests serviced. Stop accounts.
      stopAccounts(accountMbs)
    receive Reply() from self ->
      teller_loop(self, accountMbs)
  }
}

def stopAccounts(accs: [AccountMb!]) : Unit {
    caseL accs : [AccountMb!] of {
        nil -> ()
        | (a cons as) ->
            a ! Stop();
            stopAccounts(as)
        }
}

## Account process handling credit requests issued by the teller, and debit
## requests issued by other accounts.
def account(self: AccountMb?, id: Int, balance: Int): Unit {
  guard self: (Debit + Credit)* . Stop {
    free ->
      ()
    receive Debit(accountMb, amount) from self ->

      accountMb ! Done();
      account(self, id, balance + amount)
    receive Credit(tellerMb, amount, accountMb) from self ->

      # A more uglier implementation would have been to use the 'global mailbox
      # way' where all messages are collected in one mailbox.
      let transMb = new [AccountMb] in
      accountMb ! Debit(transMb, amount);

      guard transMb: Done + 1{
        free ->
          account(self, id, balance)
        receive Done() from transMb ->
          free(transMb);
          tellerMb ! Reply();
          account(self, id, balance - amount)
      }

    receive Stop() from self ->
      account_exit(self)
  }
}

## Actor process exit procedure that flushes potential residual messages.
def account_exit(self: AccountMb?): Unit {
  guard self: (Debit + Credit)*  {
    free -> ()
    receive Debit(accountMb, amount) from self ->
      account_exit(self)
    receive Credit(tellerMb, amount, accountMb) from self ->
      account_exit(self)
  }
}

## Launcher.
def main(): Unit {
  let tellerMb = new [TellerMb] in
  spawn { teller(tellerMb, 3, (200 cons (150 cons (50 cons (nil : [Int]))))) };

  tellerMb ! Start()
}

main()
