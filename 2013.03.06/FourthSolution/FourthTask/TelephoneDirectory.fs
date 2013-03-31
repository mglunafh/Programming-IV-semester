module TelephoneDirectory

type record = {name : string; number : string}
let createRecord nam num = {name = nam; number = num} 
let readLine = System.Console.ReadLine 

let rec cycle input container =
    match input with 
    | "1" -> 
        printf "Enter name:"
        let t = readLine () |> createRecord
        let pair = readLine () |> t
        startNew (pair :: container) ()
    | "2" -> ()
        
    | "3" -> ()
    | "4" -> ()
    | "5" -> ()
    | _ -> ()

and startNew container () = 
    let input = System.Console.ReadLine ()
    if (input = "0") then () else f input container

