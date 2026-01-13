interface Client { AuthDenied(), AuthGranted(), Challenge(String, ChallengeResponseHandler!) }
interface Server { LoginRequest(String, String, Client!) }
interface ChallengeResponseHandler { ChallengeResponse(String, Client!) }

def checkDetails(username: String, password: String): Bool {
    true
}

def responseValid(response: String): Bool {
    true
}

def genChallengeKey(): String {
    "challenge"
}

def checkChallengeResponse(key: String, response: String): Bool {
    true
}

def challengeResponseHandler(key: String, self: ChallengeResponseHandler?): Unit {
    guard self : ChallengeResponse {
        receive ChallengeResponse(response, replyTo) from self ->
            if (responseValid(response)) {
                replyTo ! AuthGranted()
            } else {
                replyTo ! AuthDenied()
            };
            free(self)
    }
}

def server(self: Server?): Unit {
    guard self : LoginRequest*  {
        free -> ()
        receive LoginRequest(username, password, replyTo) from self ->
            if (checkDetails(username, password)) {
                let key = genChallengeKey() in
                let responseMb = new[ChallengeResponseHandler] in
                spawn { challengeResponseHandler(key, responseMb) };
                replyTo ! Challenge(key, responseMb);
                server(self)
            } else {
                replyTo ! AuthDenied();
                server(self)
            }
    }
}

def challengeResponse(challenge: String): String {
    "challenge"
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
        receive Challenge(key, replyTo) from self ->
            print("Challenge!");
            replyTo ! ChallengeResponse(challengeResponse(key), self);
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
