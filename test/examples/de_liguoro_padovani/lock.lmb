interface Lock { Acquire(User!), Release(Unit) }
interface User { Reply(Lock!) }

def freeLock(self: Lock?): Unit {
    guard (self) : *Acquire  {
        free -> ()
        receive Acquire(owner) from self ->
            busyLock(self, owner)
        receive Release(x) from self ->
            fail(self)[Unit]
    }
}

def busyLock(self: Lock?, owner: User!): Unit {
    owner ! Reply(self);
    guard (self) : (*Acquire).Release {
        receive Release(x) from self ->
            freeLock(self)
    }
}

def user(num: Int, lock: Lock!): Unit {
    let self = new[User] in
    lock ! Acquire(self);
    guard(self) : Reply {
        receive Reply(lock) from self ->
            print(intToString(num));
            lock ! Release(());
            free(self)
    }
}


def main(): Unit {
    let lock = new[Lock] in
    spawn { freeLock(lock) };
    spawn { user(1, lock) };
    spawn { user(2, lock) }
}


main()
