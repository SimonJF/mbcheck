interface User[A] {
	Reply(A)
}

interface Future[A] {
	Put(A),
	Get(User[A]!)
}

def emptyFuture[A](self : Future[A]?) : Unit {
	guard self : Put . Get* {
		receive Put(value) from self ->
			fullFuture[A](value, self)
	}
}

def fullFuture[A](value : A, self : Future[A]?) : Unit {
	guard self : Get* {
		free -> ()
		receive Get(sender) from self ->
			sender ! Reply(value);
			fullFuture[A](value, self)
	}
}

def client[A](value : A) : Unit {
	let futureBox = new[Future[A]] in
	let self = new[User[A]] in
	spawn { emptyFuture[A](futureBox) };
	futureBox ! Put(value);
	futureBox ! Get(self);
	futureBox ! Get(self);
	futureBox ! Get(self);
	guard self : Reply . Reply . Reply {
		receive Reply(v1) from self ->
			print("1st receive");
			guard self : Reply . Reply {
				receive Reply(v2) from self ->
					print("2nd receive");
					guard self : Reply {
						receive Reply(v3) from self ->
							free(self);
							print("3rd receive")
					}
			}
	}
}

let test = spawn { client[Int](10) }; spawn { client[Int](5) }; spawn { client[Bool](true) } in ()
