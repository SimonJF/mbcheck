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

def client[A](value : A) : Unit {
	let mirrorBox = new[Mirror[A]] in
	let self = new[User[A]] in
	spawn { mirror[A](mirrorBox) };
	mirrorBox ! Put(value, self);
	guard self : Reply {
		receive Reply(v) from self ->
			free(self);
			print("received")
	}
}

def intClient() : Unit {
	client[Int](0)
}
