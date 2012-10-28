(*Nontail-recursive factorial*)
let rec fact n = if n > 0 then n * fact (n - 1) else 1 

(*Tail-recursive calculation of Fibonacci numbers*)
let rec fibTailRec n acc1 acc2 = if n > 1 then fibTailRec (n - 1) acc2 (acc1 + acc2)  else acc2
let fib n = fibTailRec n 0 1

(*Inversion of a list*)
let rev list =
    let rec rev_append l accList = match l with 
        |head :: tail -> rev_append tail (head :: accList)
        |[] -> accList
    rev_append list []


(*Function returning a list of 2^n*)
let binaryPowers n = 
    let rec f i accNumber accList = if i < n then f (i + 1) (2 * accNumber) ((2 * accNumber) :: accList) else accList
    f 0 1 []

(*Product of digits of given number*)
let productOfDigits n = 
    let rec productAux result n' = if n' > 0 then productAux (result * (n' % 10)) (n' / 10) else result
    productAux 1 n

(*Corresponds to number its first entry in the given list. Maybe the good practice is to use exception here*)
let rec firstEntryRec list n i = match list with
    |head :: tail -> if head = n then i else firstEntryRec tail n (i+1)
    |[] -> i

let firstEntry list n = firstEntryRec list n 0
printf "\n %d \n" (firstEntry [3;14;15;92;6;9] 6)

(*Checks, if the string is a palindrome*)
let string = "1ahahaha1"
let isPalindrome str = 
    let t = String.length str
    let rec f i = if i > t/2 then true else 
        if str.[i] = str.[t - i - 1] then f (i+1) else false
    f 1
  

(*point-free version func x l = List.map (fun y -> x * y) l *)
let func = List.fold ( * )