interface Recv {
	Put(Bool)
}

def boolRecv(self : Recv?) : Unit {
	guard self : Put {
		receive Put(b) from self ->
			free(self);
			print("received")
	}
}

def boolClient() : Unit {
	let mb = new[Recv] in
	spawn { boolRecv(mb) };
	mb ! Put(true)
}
