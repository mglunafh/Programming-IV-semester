open System.ComponentModel

let mutable threadResult = 0
let mutable dumbResult = 0

let countOnes threadCount multiplyer = 
    let buffer = Array.init (threadCount * multiplyer) (fun i -> 1)
    let threadFlags = Array.init threadCount (fun i -> 0)
    Array.iter (fun i -> dumbResult <- dumbResult + 1) buffer

    for i in 0..threadCount do 
        let worker = new BackgroundWorker()
        worker.DoWork.Add(fun args -> 
            let rec addPart j = if j < multiplyer then threadResult <- threadResult + buffer.[multiplyer * i + j]; addPart (j + 1)
            addPart 0)
        worker.RunWorkerCompleted.Add(fun args -> threadFlags.[i] <- 1; printfn "%d %d" i threadFlags.[i])
        worker.RunWorkerAsync()
       
       
    let mutable finallyFinished = false
    while (not finallyFinished) do
        finallyFinished <- Array.forall ((=) 1) threadFlags
        printfn "i'm wasting processor time successfully"

    printfn "%d vs %d" threadResult dumbResult