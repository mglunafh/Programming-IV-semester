module First

(* Our goal is to find a position in the given list with an option: sum of elements on previous and following positions must be maximal *)
let пробелВНазвании list = 
(*  'previous' is an element before the current or zero, 
    'acc' is a maximal sum of such kind,  
    'pos' will be returned in the end
    'curPos' -- self-explanatory *)
    let rec f1 l previous acc pos curPos = 
        match l with
        |l1 :: l2 :: ls -> 
            let curAcc = previous + l2 
            if (curAcc > acc) then 
                f1 (l2 :: ls) l2 curAcc curPos (curPos + 1)
            else
                f1 (l2 :: ls) l2 curAcc pos (curPos + 1)
        |l1 :: [] -> if (previous > acc) then curPos else pos 
        |[] -> 0
    f1 list 0 -1048576 0 0 

printf "%d " (пробелВНазвании [1; 4; 8; 8])