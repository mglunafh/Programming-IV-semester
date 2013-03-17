module PointFree

(* let func x l = List.map (fun y -> y * x) l *)
let f1 x = List.map (fun y -> y * x) 
let f2 x = List.map (( * ) x)
let f3 x = (List.map << ( * )) x  
let f4 = List.map << ( * )