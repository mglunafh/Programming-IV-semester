module Palindrome
// to find a palindrome number which can be obtained by product of a couple of three-digit numbers.

let isPalindrome n =
    let rec digits number i acc = 
        if number > 0 then 
            digits (number / 10) (i + 1) ((number % 10) :: acc) 
        else 
            (acc, List.rev acc)
    
    let rec check lists = 
        match lists with
        | (hd1 :: tl1, hd2 :: tl2) -> if hd1 = hd2 then check (tl1, tl2) else false
        | ([], []) -> true
        | (_, _) -> failwith "Потеряли элементы, пока реверсили список, .NET отстой"

    check (digits n 0 [])

let lookingForMost index1 index2 =
    let rec lookingFor ind1 ind2 acc = 
        let nextAcc = 
            let t = ind1 * ind2 
            if isPalindrome t && t > acc then t else acc
        if ind1 < 999 then 
            lookingFor (ind1 + 1) ind2 nextAcc
        elif ind2 < 999 then 
            lookingFor (ind2 + 1) (ind2 + 1) nextAcc
        else 
            acc
    lookingFor index1 index2 0