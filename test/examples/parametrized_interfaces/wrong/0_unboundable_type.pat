interface Recv[A, Int] {
	Put(Int),
	PutOpen(A)
}

def intRecv(self : Recv[Int, Int]?) : Unit {
	guard self : Put {
		receive Put(x) from self->
			free(self)
	}
}

def client() : Unit {
	let mb = new[Recv[Int, Int]] in
	spawn { intRecv(mb) };
	mb ! Put(2)
}
