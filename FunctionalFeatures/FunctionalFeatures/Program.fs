let rec fact n = if n > 0 then n * fact (n - 1) else 1 
printf "Hello world! 10! is %d \n" (fact 10)

let rec fibTailRec n acc1 acc2 = if n > 1 then fibTailRec (n - 1) acc2 (acc1 + acc2)  else acc2
let fib n = fibTailRec n 0 1
printf "These are %d \n" (fib 10)

let rev list =
    let rec rev_append l accList = match l with 
        |head :: tail -> rev_append tail (head :: accList)
        |[] -> accList
    rev_append list []

let print list = List.iter (fun x -> printf "%d " x) (rev list)

print [1; 4; 8; 14; 88]

let binaryPowers n = 
    let rec f i accNumber accList = if i < n then f (i + 1) (2 * accNumber) ((2 * accNumber) :: accList) else accList
    f 0 1 []
print (binaryPowers 5)

let productOfDigits n = 
    let rec productAux result n' = if n' > 0 then productAux (result * (n' % 10)) (n' / 10) else result
    productAux 1 n

printf "\n%d\n" (productOfDigits 1349)

let rec firstEntryRec list n i = match list with
    |head :: tail -> if head = n then i else firstEntryRec tail n (i+1)
    |[] -> i

let firstEntry list n = firstEntryRec list n 0
printf "\n %d \n" (firstEntry [3;14;15;92;6;9] 6)

let string = "1ahahaha1"
let palindrome str = 
    let t = String.length str
    let rec f i = if i > t/2 then true else 
        if str.[i] = str.[t - i - 1] then f (i+1) else false
    f 1

printf "\n%c %c %d %c\n" string.[1] string.[2] (String.length string) string.[6]

printf "%s is %sa palindrome\n" string (if palindrome string  then "" else "not ")
  

//point-free version func x l = List.map (fun y -> x * y) l
let func = List.fold ( * )