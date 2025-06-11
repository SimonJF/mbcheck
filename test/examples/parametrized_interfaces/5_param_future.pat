interface User<x> {
	Reply(x)
}

interface Future<x> {
	Put(x),
	Get(User<x>!)
}

def emptyFuture<x>(self : Future<x>?) : Unit {
	guard self : Put . Get* {
		receive Put(value) from self ->
			fullFuture<x>(value, self)
	}
}

def fullFuture<x>(value : x, self : Future<x>?) : Unit {
	guard self : Get* {
		free -> ()
		receive Get(sender) from self ->
			sender ! Reply(value);
			fullFuture<x>(value, self)
	}
}

def intClient() : Unit {
	let futureBox = new[Future<Int>] in
	let self = new[User<Int>] in
	spawn { emptyFuture<Int>(futureBox) };
	futureBox ! Put(0);
	futureBox ! Get(self);
	guard self : Reply {
		receive Reply(value) from self ->
			free(self);
			print("received")
	}
}
