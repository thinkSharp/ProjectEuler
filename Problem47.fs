module Problem47
open System

// Problem 47 -- Find the first four consecutive integers to have four distinct primes factors
let isPrimeSqrt n =
    let rec check i =
        i > int64(sqrt(double(n))) || (n % i <> 0L && check (i + 1L))
    if n = 1L then false else check 2L   

let getprimeFactors num (primes:int64 [])=
    let rec innerLoop rem count result =
        match isPrimeSqrt rem with
         | true -> (result + " x " + rem.ToString())
         | _ -> let isPrimeFactor = rem % primes.[count] = 0L
                let newRem, newCount, newRes = if isPrimeFactor then rem / primes.[count], count, (if result = "" then  primes.[count].ToString() else result + " x " + primes.[count].ToString()) else rem ,(count + 1), result
                innerLoop newRem newCount newRes
    innerLoop num 0 ""

let getprimeFactorsUnique num (primes:int64 [])=
    let rec innerLoop rem count (result:Set<int64>) =
        match isPrimeSqrt rem with
         | true -> result.Add(rem)
         | _ -> let isPrimeFactor = rem % primes.[count] = 0L
                let newRem, newCount, newRes = if isPrimeFactor then rem / primes.[count], count, result.Add(primes.[count]) else rem ,(count + 1), result
                innerLoop newRem newCount newRes
    innerLoop num 0 Set.empty

let getConsecutiveNumbersWithPrimeFactor num = 
   let primes = [|2L .. 1000L |] |> Array.filter(fun elm -> isPrimeSqrt elm)
   //getprimeFactorsUnique num primes
   let rec getConsNumbersWithPrime count (result:Set<int64>) =
            match result.Count = num with
             | true -> result
             | _ -> let primeFactors = getprimeFactorsUnique count primes
                    let newRes = if primeFactors.Count = num then result.Add(count) else Set.empty
                    getConsNumbersWithPrime (count + 1L) newRes
   let vals = getConsNumbersWithPrime 2L Set.empty
   vals |> Set.iter(fun elm -> printf "%d \n" elm )
   vals                 

// Problem 47 -- Find the first four consecutive integers to have four distinct primes factors

