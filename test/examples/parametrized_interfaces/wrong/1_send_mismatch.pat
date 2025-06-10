interface Recv<x> {
	Put(x)
}

def boolRecv(self : Recv<Bool>?) : Unit {
	guard self : Put {
		receive Put(b) from self ->
			free(self);
			print("received")
	}
}

def boolClient() : Unit {
	let mb = new[Recv<Bool>] in
	spawn { boolRecv(mb) };
	mb ! Put(1)
}
