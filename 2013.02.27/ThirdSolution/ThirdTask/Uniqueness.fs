module Uniqueness

let unique list =
    let rec uniqueTailRec l acc = 
        match l with
        |hd :: tl -> uniqueTailRec tl (acc || List.exists ((=) hd) tl)
        |[] -> acc
    not <| uniqueTailRec list false

