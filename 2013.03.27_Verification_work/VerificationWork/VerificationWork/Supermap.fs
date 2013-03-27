module Supermap

let flatten list =
    let rec flatten_aux l acc = 
        match l with 
        | l1 :: ls -> flatten_aux ls (acc @ l1)
        | [] -> acc
    flatten_aux list []

let superMap list f = flatten (List.map f list)