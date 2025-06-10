interface User<x> {
	Reply(x)
}

interface Mirror<x> {
	Put(x, User<x>!)
}

def boolMirror(self : Mirror<Bool>?) : Unit {
	guard self : Put {
		receive Put(msg, sender) from self ->
			free(self);
			sender ! Reply(msg)
	}
}

def boolClient() : Unit {
	let mirrorBox = new[Mirror<Bool>] in
	let self = new[User<Bool>] in
	spawn { boolMirror(mirrorBox) };
	mirrorBox ! Put(true, self);
	guard self : Reply {
		receive Reply(x) from self ->
			free(self);
			print("received")
	}
}

def intMirror(self : Mirror<Int>?) : Unit {
	guard self : Put {
		receive Put(msg, sender) from self ->
			free(self);
			sender ! Reply(msg)
	}
}

def intClient() : Unit {
	let mirrorBox = new[Mirror<Int>] in
	let self = new[User<Int>] in
	spawn { intMirror(mirrorBox) };
	mirrorBox ! Put(0, self);
	guard self : Reply {
		receive Reply(x) from self ->
			free(self);
			print("received")
	}
}
