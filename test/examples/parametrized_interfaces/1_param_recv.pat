interface Recv[A] {
	Put(A)
}

def boolRecv(self : Recv[Bool]?) : Unit {
	guard self : Put {
		receive Put(b) from self ->
			free(self);
			print("received")
	}
}

def boolClient() : Unit {
	let mb = new[Recv[Bool]] in
	spawn { boolRecv(mb) };
	mb ! Put(true)
}

def intRecv(self : Recv[Int]?) : Unit {
	guard self : Put {
		receive Put(b) from self ->
			free(self);
			print("received")
	}
}

def intClient() : Unit {
	let mb = new[Recv[Int]] in
	spawn { intRecv(mb) };
	mb ! Put(0)
}
