module TelephoneDirectory

open System.IO

type record = {name : string; number : string}
let createRecord nam num = {name = nam; number = num} 
let readLine = System.Console.ReadLine 

let rec cycle input container =
    match input with 
    | "1" -> 
        printf "Enter name:"
        let t = readLine () |> createRecord
        printf "Enter number:"
        let pair = readLine () |> t
        startNew (pair :: container) ()
    
    | "2" -> 
        let givenName = readLine ()
        let result = List.tryFind (fun x -> x.name = givenName) container
        printf "%A \n" result 
        startNew container ()
    
    | "3" -> 
        let givenNumber = readLine ()
        let result = List.tryFind (fun x -> x.number = givenNumber) container
        printf "%A \n" result 
        startNew container ()
    
    | "4" -> 
        let saveRecords (fileName : string) = 
            use dataFile = new StreamWriter(fileName)
            let rec save recordList = 
                match recordList with
                | hd :: tl -> dataFile.WriteLine (hd.name + " " + hd.number + "\n")
                | [] -> ()
            save container

        saveRecords "data.txt"
        startNew container ()

    | "5" ->
        let fileContent = File.ReadLines(@"data.txt") |> Seq.toList
        let recordList = List.map (fun (x : string)-> let array = x.Split([|' '|]) in createRecord array.[0] array.[1]) fileContent
        
        startNew (container @ recordList) ()
    | _ -> startNew container ()

and startNew container () = 
    let input = System.Console.ReadLine ()
    if (input = "0") then () else cycle input container

