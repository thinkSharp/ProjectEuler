module Problem50
open System

// Problem 50 -- Which prime, below one-million, can be written as the sum of the most consecutive primes?
let isprimeSqrt n =
    let rec check i =
        i > int(sqrt(double(n))) || (n % i <> 0 && check (i + 1))
    check 2  

let getSumOfMostConsecutivePrimes limit =
    let primes = [|2 .. limit |]
                 |> Array.filter(fun elm -> isprimeSqrt elm)
    
    let rec getSumOfMostConsecutivePrime start count runningTotal result =
        match count = primes.Length || runningTotal > limit with
         | true -> result
         | _ -> let newRunningTotal = runningTotal + primes.[count]
                let newResult = if isprimeSqrt newRunningTotal then (newRunningTotal,(count + 1 - (start - 1))) else result
                //printf "%d %d %d \n " count newRunningTotal (fst newResult)
                getSumOfMostConsecutivePrime start (count + 1) newRunningTotal newResult
                   
    let rec getSumOfConsecutivePrime start result =
        match start = primes.Length with
         | true -> result 
         | _ -> let tempResult = getSumOfMostConsecutivePrime start start primes.[start - 1] (primes.[start - 1], 1)
                let newResult = if (snd tempResult >  snd result) && (fst tempResult ) <= limit then tempResult else result
                printf "%d %d %d \n" start (snd newResult) (fst newResult)
                getSumOfConsecutivePrime (start + 1) newResult
    getSumOfConsecutivePrime 1 (2,1)
    
// Problem 50 -- Which prime, below one-million, can be written as the sum of the most consecutive primes?

