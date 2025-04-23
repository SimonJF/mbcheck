### Adapted from savina/barber.
###
### A barber is waiting for customers. When there are no customers the barber sleeps. When a customer
### arrives, he wakes the barber or sits in one of the waiting chairs in the waiting room. If all chairs
### are occupied, the customer leaves. Customers repeatedly visit the barber shop until they get a haircut.
### The key element of the solution is to make sure that whenever a customer or a barber checks the state
### of the waiting room, they always see a valid state. The problem is implemented using a Selector actor
### that decides which is the next customer, Customer actors, a Barber actor and a Room actor.

interface RoomMb {
    Enter(CustomerMb!, RoomMb!),
    Next(),
    Exit()
}

interface BarberMb {
    Enter(CustomerMb!, RoomMb!),
    Wait(),
    Exit()
}

interface SelectorMb {
    Start(),
    Returned(CustomerMb!),
    Done()
}

interface CustomerMb {
    Full(),
    Wait(),
    Start(),
    Done()
}

def room(self: RoomMb?, capacity: Int, barber: BarberMb!): Unit {

}

def barber(self: BarberMb?): Unit {

}

def selector(self: SelectorMb?, generator: Int, haircuts: Int, room: RoomMb!): Unit {

}

def customer(self: CustomerMb?, selector: SelectorMb!): Unit {

}
