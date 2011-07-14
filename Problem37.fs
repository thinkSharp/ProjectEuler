module Problem37
open System

// Problem 37 -- find the sum of eleven primes that are both truncatable from left to right and right to left and are still primes
let isPrimeSqrt n =
    let rec check i =
        i > int(sqrt(double(n))) || (n % i <> 0 && check (i + 1))
    if n = 1 then false else check 2   


let isLeftTruncatablePrimes prime =
    let limit = prime.ToString().Length
    let primeStr = prime.ToString()
    let rec interLoop count result =
        match count = limit || result = false with
         | true -> result
         | _ -> let newPrime = primeStr.Substring(count)
                //printf "%d %s %b \n" count newPrime result         
                interLoop (count + 1) (isPrimeSqrt (System.Convert.ToInt32(newPrime)))
    interLoop 1 true
    
let isRightTruncatablePrimes prime =
    let limit = prime.ToString().Length
    let primeStr = prime.ToString()
    let rec interLoop count result =
        match count = 0 || result = false with
         | true -> result
         | _ -> let newPrime = primeStr.Substring(0, count)
                //printf "%d %s %b \n" count newPrime result         
                interLoop (count - 1) (isPrimeSqrt (System.Convert.ToInt32(newPrime)))
    interLoop (limit - 1) true

let returnOption f n =
    match f n with
     | true -> Some(true)
     | false -> None

let getAllTruncatablePrimesUnder limit limitCount =
     let x = [|10 .. limit|]
            |> Array.filter(fun elm -> elm % 2 <> 0)
            |> Array.filter(fun elm -> elm % 3 <> 0)
            |> Array.filter(fun elm -> elm % 5 <> 0)
            |> Array.filter(fun elm -> elm % 7 <> 0)

     let rec interLoop count valueCount sum =
        match valueCount = limitCount with
         | true -> sum
         | _ -> if isPrimeSqrt x.[count] && isLeftTruncatablePrimes x.[count] && isRightTruncatablePrimes x.[count] then
                    printf "%d %d \n" x.[count] sum
                    interLoop (count + 1) (valueCount + 1) sum + x.[count]
                else
                    interLoop (count + 1) valueCount sum

     interLoop 0 0 0

// Problem 37 -- find the sum of eleven primes that are both truncatable from left to right and right to left and are still primes

