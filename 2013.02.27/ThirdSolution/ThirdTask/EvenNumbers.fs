module EvenNumbers

/// three implementations of function counting even numbers in list 
/// map, filter, and fold 

let (%) a b = let dividend = a / b in (a - dividend * b)

let sum = List.fold (+) 0
let first l = sum (List.map (fun x -> 1 - (x % 2)) l)

let second l = List.length (List.filter (fun x -> x % 2 = 0) l)
                                                          
let third = List.fold (fun a b -> a + (1 - (b % 2))) 0

let list = [3; 1; 4; 2; 7; 4; 2; 8; 3; 6; 0; 2; 1]

