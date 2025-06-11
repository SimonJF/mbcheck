interface User<x> {
	Reply(x)
}

interface Future<x> {
	Put(x),
	Get(User<x>!)
}

def emptyFuture<x>(self : Future<x>?) : Unit {
	guard self : Put . Get* {
		receive Put(value) from self ->
			fullFuture<x>(value, self)
	}
}

def fullFuture<x>(value : x, self : Future<x>?) : Unit {
	guard self : Get* {
		free -> ()
		receive Get(sender) from self ->
			sender ! Reply(value);
			fullFuture<x>(value, self)
	}
}

def client<x>(value : x) : Unit {
	let futureBox = new[Future<x>] in
	let self = new[User<x>] in
	spawn { emptyFuture<x>(futureBox) };
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

let test = spawn { client<Int>(10) }; spawn { client<Int>(5) }; spawn { client<Bool>(true) } in ()
