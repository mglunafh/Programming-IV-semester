module Uniqueness

let unique list =
    let rec uniqueTailRec l acc = 
        match l with
        |hd :: tl -> uniqueTailRec tl (acc || List.exists ((=) hd) tl)
        |[] -> acc
    not <| uniqueTailRec list false

printf "%b %b " (unique [1; 4; 8; 8]) (unique [1; 3; 4; 9])