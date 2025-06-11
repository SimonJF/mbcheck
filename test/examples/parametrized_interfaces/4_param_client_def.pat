interface User<x> {
	Reply(x)
}

interface Mirror<x> {
	Put(x, User<x>!)
}

def mirror<x>(self : Mirror<x>?) : Unit {
	guard self : Put {
		receive Put(msg, sender) from self ->
			free(self);
			sender ! Reply(msg)
	}
}

def client<x>(value : x) : Unit {
	let mirrorBox = new[Mirror<x>] in
	let self = new[User<x>] in
	spawn { mirror<x>(mirrorBox) };
	mirrorBox ! Put(value, self);
	guard self : Reply {
		receive Reply(v) from self ->
			free(self);
			print("received")
	}
}

def intClient() : Unit {
	client<Int>(0)
}
