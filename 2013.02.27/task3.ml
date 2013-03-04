open List;;
open Printf;;


(* three implementations of function counting even numbers in list
map, filter, fold *)


let (%) a b = let dividend = a / b in (a - dividend * b);;

let sum = fold_left (+) 0;;
let first l = sum (map (fun x -> 1 - (x % 2)) l);;

let second l = length (filter (fun x -> x % 2 = 0) l);; 
                                                          
let third = fold_left (fun a b -> a + (1 - (b % 2))) 0;;

let list = [3;1;4;2;7;4;2;8;3;6;0;2;1];;

printf "%d %d %d" (first list) (second list) (third list);
 
