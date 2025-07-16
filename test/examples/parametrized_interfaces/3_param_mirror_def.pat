interface User[A] {
	Reply(A)
}

interface Mirror[A] {
	Put(A, User[A]!)
}

def mirror[A](self : Mirror[A]?) : Unit {
	guard self : Put {
		receive Put(msg, sender) from self ->
			free(self);
			sender ! Reply(msg)
	}
}

def client() : Unit {
	let boolMirrorBox = new[Mirror[Bool]] in
	let self = new[User[Bool]] in
	spawn { mirror[Bool](boolMirrorBox) };
	boolMirrorBox ! Put(true, self);
	guard self : Reply {
		receive Reply(x) from self ->
			free(self);
			print("received")
	};
	let intMirrorBox = new[Mirror[Int]] in
	let self = new[User[Int]] in
	spawn { mirror[Int](intMirrorBox) };
	intMirrorBox ! Put(0, self);
	guard self : Reply {
		receive Reply(x) from self ->
			free(self);
			print("received")
	}
}
