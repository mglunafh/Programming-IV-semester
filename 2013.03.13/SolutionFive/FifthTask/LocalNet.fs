module LocalNet

open System.IO

type OperationSystem =      
    |Windows = 56 
    |Linux = 15 
    |MacOS = 5

type Computer = 
    {number : int; 
    OS : OperationSystem; 
    mutable isInfected : bool; 
    mutable infectedJustNow : bool}     // computer with 'true' here should wait one turn 
                                        // before 'infestation' is unlocked

type Net = {computers : Computer []; graph : int [] []}

let infestation net = 
    // trying to infect all my neighbours
    let infect comp =
        let rand = new System.Random()
        for i = 0 to Array.length net.graph do
            // here we attempt to exploit our neighbour's primary orbital and antivirus defenses
            if (net.graph.[comp.number].[i] = 1 && rand.Next(100) > (int comp.OS))
            then 
                net.computers.[i].isInfected <- true
                net.computers.[i].infectedJustNow <- true

     // this function permits all infected units to procreate the epidemy 
    let nextTurn = Array.iter (fun (x : Computer) -> x.infectedJustNow <- false)
    
    Array.iter (fun x -> if (x.isInfected && not x.infectedJustNow) then infect x) net.computers
    nextTurn net.computers
    net

let showInfo (net : Net) = Array.iteri (fun i x -> printfn "Computer №%d %b" i x.isInfected)  net.computers

let letsRun net = 
    net |> infestation |> showInfo
    while true do
        System.Console.WriteLine("Run next step?(y/_)\n") 
        match (System.Console.ReadLine()) with
        |"y" -> net |> infestation |> showInfo
        |_ -> ()
        