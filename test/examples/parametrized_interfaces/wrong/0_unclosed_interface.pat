interface Recv {
	Put(Int),
	PutOpen(A)
}

def intRecv(self : Recv?) : Unit {
	guard self : Put {
		receive Put(x) from self->
			free(self)
	}
}

def client() : Unit {
	let mb = new[Recv] in
	spawn { intRecv(mb) };
	mb ! Put(2)
}
