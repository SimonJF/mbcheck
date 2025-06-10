interface Recv<a> { Put(a) }

def boolRecv(self : Recv<bool>?) : Unit {
	guard self : Recv<bool> {
		receive Put(b) from self' ->
			free self;
			if b then print("true") else print("false")
	}
}

def boolClient() : Unit {
	let mb = new[Recv<bool>] in
	spawn { boolRecv(mb) };
	mb ! true
}
