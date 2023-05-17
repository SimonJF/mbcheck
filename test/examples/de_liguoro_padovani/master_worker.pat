
# In Pat, mailbox interfaces are defined from the point of view of the
# receiving end of the mailbox.


# Mailbox message interfaces.
interface MasterMb {
    Task(ClientMb!, Int)
}

interface PoolMb {
    Result(Int)
}

interface WorkerMb {
    Work(PoolMb!, Int)
}

interface ClientMb {
    Result(Int)
}

# Master server loop handling incoming client tasks.
#
# self: Master mailbox where client tasks are deposited.
#
# Returns: Unit value.
def master(self: MasterMb?): Unit {
    guard self: *Task {
        free -> () # No more tasks to handle.    
        receive Task(replyTo, n) from self ->
            
            # Create a local throwaway mailbox used by the master to farm tasks
            # and collect results.
            let pool = new[PoolMb] in
            
            # Farm the number of tasks 'n'.
            farm(0, n, pool);

            # Block until all the results are computed by each worker and 
            # communicate result to client.  
            let result = harvest(0, pool) in
            replyTo ! Result(result);

            # Service next task in mailbox.
            master(self)
    }
}

# Worker computing assigned task by master.
#
# self: Worker mailbox where task is deposited.
#
# Returns: Unit value.
def worker(self: WorkerMb?): Unit {
    guard self: Work {
        receive Work(replyTo, n) from self ->
            replyTo ! Result(compute(n));
            free(self)
    }        
}

# Distributes tasks between worker processes.
#
# count: Current task index/value.
# chunks: Number of segments the client task consists of.
# pool: Mailbox to which results from workers are to be communicated.
#
# Returns: Unit value.
def farm(count: Int, chunks: Int, pool: PoolMb!): Unit {
    if (count == chunks) {
        # chunks
        ()
    }
    else {
        
        # Fabricate simple work task chunk to assign to worker.
        let task = count + 1 in

        # Create worker and assign chunk.
        let workerMb = new[WorkerMb] in
        spawn { worker(workerMb) };
        workerMb ! Work(pool, task);

        farm(task, chunks, pool)
    }
}

# Collects and sums the individual results of the tasks assigned to workers.
#
# count: Current task index.
# chunks: Number of work result chunks to expect. Dead parameter for not (see 
#         question below).
# acc: Accumulator holding the intermediate summation of results from workers.
# pool: Mailbox where the worker results are deposited.
#
# Returns: Accumulated total.
def harvest(acc: Int, pool: PoolMb?): Int {
    guard pool: *Result {
        free -> 
            # We do not keep track of the expected number of chunks since this
            # is something that is done automatically by the runtime. The fact
            # that we create a local mailbox pool enables the runtime to track
            # the number of times the mailbox has been shared with processes.
            # By reference counting, the runtime is able to assert that when the
            # free branch becomes available, there are no more results in the 
            # pool mailbox, which implies that all the work chunk replies have 
            # been accounted for, and none where lost or duplicated. The 
            # commented code shown below re-performs this check, and is 
            # redundant.
            acc 
                
        receive Result(n) from pool ->
            harvest(acc + n, pool)
    }
}

# What if I want to exactly count the number of replies I expect. This is more
# stringent than merely using *. Can I do this? Or rather, am I prevented from 
# doing so?

# Models a complex computation that a worker performs.
#
# n: Some task.
#
# Returns: Result. 
def compute(n: Int): Int {
    n * n
}

# Client issuing one (numerical) task to the master.
#
# n: Some task.
# self: Mailbox where the result from the master is deposited.
# masterMb: Master mailbox to where the request is directed.
#
# Returns: Unit value.
def client(n: Int, self: ClientMb?, masterMb: MasterMb!): Unit {
    masterMb ! Task(self, n);
    guard self: Result {
        receive Result(result) from self ->
            free(self);
            print(intToString(result))
    }
}

# Launcher.
def main(): Unit {
    
    let masterMb = new[MasterMb] in
    spawn { master(masterMb) };

    let client1 = new[ClientMb] in
    spawn { client(5, client1, masterMb) };

    let client2 = new[ClientMb] in
    spawn { client(10, client2, masterMb) }
}

main()
