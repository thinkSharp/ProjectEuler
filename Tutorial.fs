
// F# Tutorial File
module Tutorial
open System
    
let isPrime x =
    let mutable answer = [false;]
    let sqNum = int64(sqrt(double(x)))
    
    for i in 2L .. sqNum do
        let value = x % i = 0L 
        //printfn "%b" value
        let valueL = value :: []
        answer <- answer @ valueL
    
    let prime = List.exists (fun elm -> elm) answer
    not prime 

let getPrimeNumbers x =
   let primNums = {2L .. x}
                   |> Seq.filter (fun elm -> elm % 2L <> 0L)
                   |> Seq.filter (fun elm -> elm % 3L <> 0L)
                   |> Seq.filter (fun elm -> elm % 5L <> 0L)
                   |> Seq.filter (fun elm ->isPrime elm)
   Seq.append {2L .. 3L}  primNums

let largestPrimeFactor x elem acc =
    if x% elem = 0L then
        //printf "%d" elem
        elem
    else
        acc

let getLargestPrimeFactor x =
    let getPrimNums = getPrimeNumbers x
                           
    Seq.fold (fun acc elem -> largestPrimeFactor x elem acc) 2L getPrimNums



let isNumberPalindrome x =
   let mutable isPal = true
   let sX = string(x)
   let lensX = sX.Length
   let halfLx = int(lensX / 2)
   for i= 0 to halfLx do
     let tailLen = lensX - i - 1
     if( sX.[i] = sX.[tailLen]) && isPal then
        isPal <- true
     else
        isPal <- false
   isPal


let getLargestPal =
    let mutable i = 1000
    let mutable j = 999
    let mutable largestPal = 0
    while (i > 99) do
        i <- i - 1
        j <- 999
        while (j > 99) do
            let mult = j * i 
            printf "%d * %d = %d" i j mult
            if (isNumberPalindrome mult) then
                i <- 99
                j <- 99
                largestPal <- mult
            else
                j <- j - 1
    largestPal

let isDivisibleBy1To20 elm =
    //elm % 2 = 0 && elm % 3 = 0 && elm % 4 = 0 && elm % 5 = 0 && elm % 6 = 0 && elm % 7 = 0 && elm % 8 = 0 && elm % 9 = 0 && elm % 10 = 0 
    elm % 11 = 0 && elm % 12 = 0 && elm % 13 = 0 && elm % 14 = 0 && elm % 15 = 0 && elm % 16 = 0 && elm % 17 = 0 && elm % 18 = 0 && elm % 19 = 0 && elm % 20 = 0 

let getSmallestIntDivisibleBy1to20 lastVal =
   let num = seq { 1 .. lastVal} 
                |> Seq.find(isDivisibleBy1To20)
   num


let getDiff x =
    let first = {1 .. x}
                 |> Seq.sum

    let sqfirst = first * first 

    let second = {1 .. x}
                   |> Seq.map(fun x -> x * x )
                   |> Seq.sum

    let ans = sqfirst - second
    ans


 


 

 


 



 


 



 

 


 




// Problem 18 -- find maximum total from top to bottom of a triangle
let triangle a =[|[|3|];[|7; 4;|];[|2; 4; 6;|];[|8; 5; 9; 3;|];|]



// Problem 18 -- find maximum total from top to bottom of a triangle
