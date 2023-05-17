### Adapted from Savina/banking.
###
### A benchmark that implements a request-reply chain passing scenario. Several 
### account processes are spawned, each of which may be sent a credit request
### by the central teller process. The benchmark is parameterized by the number 
### of account processes. Since the array type is not available in Pat, we fix 
### the number of account processes to 3.

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

## Teller process handling the creation of account processes and launching of 
## main loop.
def teller(self: TellerMb?, numAccounts: Int): Unit {
  
  # Create accounts.
  let accountMb1 = new [AccountMb] in
  spawn { account(accountMb1, 1, 200) };

  let accountMb2 = new [AccountMb] in
  spawn { account(accountMb2, 2, 150) };

  let accountMb3 = new [AccountMb] in
  spawn { account(accountMb3, 3, 50) };

  guard self: Start {
    receive Start() from self ->

      # Note. Even if the randomization of message sending is performed in a 
      # different processes, there is no risk of a race condition where the
      # messages are sent to a non-existing mailbox. This is because, as can be
      # gleaned above, mailboxes are created in sequential fashion within this
      # (i.e., the teller) process.
      spawn { generate_work(self, numAccounts, accountMb1, accountMb2, accountMb3) } ;
      teller_loop(self, accountMb1, accountMb2, accountMb3)
  }
}

## Randomly chooses the source account.
def generate_work(tellerMb: TellerMb!, numAccounts: Int, acc1: AccountMb![R], acc2: AccountMb![R], acc3 : AccountMb![R]): Unit {
  
  # Randomly choose source account from which the funds shall be taken.
  let sourceId = rand(numAccounts - 1) in # -1 because rand() is 0-indexed.
  if (sourceId == 0) {

    # First source account.
    choose_dst_acc(tellerMb, numAccounts, acc1, acc2, acc3)
  }
  else { 
    if (sourceId == 1) {

        # Second source account.
        choose_dst_acc(tellerMb, numAccounts, acc2, acc1, acc3)
    } 
    else {

        # Third source account.
        choose_dst_acc(tellerMb, numAccounts, acc3, acc1, acc2)
    }
  }
}

## Randomly chooses the destination account and issues a credit request. The
## function ensures that the source and destination account are different.
def choose_dst_acc(tellerMb: TellerMb!, numAccounts: Int, srcAccountMb: AccountMb![R], dstAccountMb1: AccountMb![R], dstAccountMb2 : AccountMb![R]): Unit {
  
  # Randomly choose destination account to which funds shall be deposited. -2 
  # because rand() is 0-indexed, and because we do not include the source 
  # account in the random choice (i.e., the source account is not permitted to
  # send funds to itself).
  let dstAccountId = rand(numAccounts - 2) in 

  let dstAccount = 
    if (dstAccountId == 0) {
      dstAccountMb1
    } else {
      dstAccountMb2
    }
  in

  let amount = rand(200) in
  dstAccount ! Credit(tellerMb, amount, srcAccountMb)
}

## Teller process main loop handling replies from accounts.
def teller_loop(self: TellerMb?, accountMb1: AccountMb!, accountMb2: AccountMb!, accountMb3: AccountMb!): Unit {
  guard self: *Reply {
    free -> 

      # All credit requests serviced. Stop accounts.
      accountMb1 ! Stop();
      accountMb2 ! Stop();
      accountMb3 ! Stop()
    receive Reply() from self ->
      teller_loop(self, accountMb1, accountMb2, accountMb3)
  }
}

## Account process handling credit requests issued by the teller, and debit 
## requests issued by other accounts.
def account(self: AccountMb?, id: Int, balance: Int): Unit {
  guard self: *(Debit + Credit) . Stop {
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

## Actor process exit procedure that flushes potential residual messages.
def account_exit(self: AccountMb?): Unit {
  guard self: *(Debit + Credit)  {
    free -> ()
    receive Debit(accountMb, amount) from self ->
      account_exit(self)
    receive Credit(tellerMb, amount, accountMb) from self ->
      account_exit(self)
  }
}

## Launcher.
def main(): Unit {
  let tellerMb = new [TellerMb] in
  spawn { teller(tellerMb, 3) };

  tellerMb ! Start()
}

main()
