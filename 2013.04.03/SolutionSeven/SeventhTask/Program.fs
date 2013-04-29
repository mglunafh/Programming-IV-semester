open System.ComponentModel

let mutable threadResult = 0
let mutable dumbResult = 0 

let countOnes multiplyer n = 
    let buffer = Array.init (n * multiplyer) (fun i -> 1)
    
    Array.iter (fun i -> dumbResult <- dumbResult + 1) buffer

    for i in 0..n do 
        let worker = new BackgroundWorker()
        do worker.DoWork.Add(fun args -> 
            let rec addPart j = if j < multiplyer then threadResult <- threadResult + buffer.[1000 * i + j]
            addPart 0)
        worker.RunWorkerAsync()

    printfn "%d vs %d" threadResult dumbResult