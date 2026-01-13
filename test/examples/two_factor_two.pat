interface Client { AuthDenied(), AuthGranted(), Challenge(String) }
interface Server { LoginRequest(String, String, Client!), ChallengeResponse(String, Client!) }

def checkDetails(username: String, password: String): Bool {
    true
}

def responseValid(response: String): Bool {
    true
}

def genChallengeKey(): String {
    "challenge"
}

def server(self: Server?): Unit {
    guard self : LoginRequest* . ChallengeResponse* {
        free -> ()
        receive LoginRequest(username, password, replyTo) from self ->
            if (checkDetails(username, password)) {
                let key = genChallengeKey() in
                replyTo ! Challenge(key)
            } else {
                replyTo ! AuthDenied()
            };
            server(self)
        receive ChallengeResponse(response, replyTo) from self ->
            if (responseValid(response)) {
                replyTo ! AuthGranted()
            } else {
                replyTo ! AuthDenied()
            };
            server(self)
    }
}

def challengeResponse(challenge: String): String {
    "challengeResponse"
}

def client(server: Server!): Unit {
    let self = new[Client] in
    server ! LoginRequest("simon", "password", self);
    guard self : AuthGranted + AuthDenied + Challenge {
        receive AuthGranted() from self ->
            print("Granted!");
            free(self)
        receive AuthDenied() from self ->
            print("Denied!");
            free(self)
        receive Challenge(key) from self ->
            print("Challenge!");
            server ! ChallengeResponse(challengeResponse(key), self);
            guard self : AuthGranted + AuthDenied {
                receive AuthGranted() from self ->
                    print("Granted!");
                    free(self)
                receive AuthDenied() from self ->
                    print("Denied!");
                    free(self)
            }
    }
}

def main(): Unit {
    let serverMb = new[Server] in
    spawn { server(serverMb) };
    client(serverMb)
}

main()
