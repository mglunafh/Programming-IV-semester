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
        let fileName = readLine()
        let fileContent = File.ReadLines(fileName) |> Seq.toList
        List.iter (printfn "%s_") fileContent 
        let recordList = 
            fileContent             |>
            List.filter ((<>) "")   |>
            List.map 
                (fun (x : string)-> 
                    let array = x.Split([|' '|]) in createRecord array.[0] array.[1]) 
        startNew (container @ recordList) ()
		
    | _ -> startNew container ()

and startNew container () = 
    let input = System.Console.ReadLine ()
    if (input = "0") then () else cycle input container

let firstEntry () = 
    let welcome = 
        "Hey there! This is a program for simple telephone directory, try and explore it!\n" +
        "Possible actions you can do in this console:\n" + 
        "1 is for creating a new record;\n" + 
        "2 is for searching record by name and\n" + 
        "3 is for searching by number;\n" + 
        "4 -- and program saves all changes in a file, you just need to enter a desired name;\n" +
        "5 is for loading records from given file\n" + 
        "0 means shutdown of program. Warning: in this version unsaved changes will be lost, once you press this digit ._."
    printfn "%s" welcome
    startNew [] ()