module Problem46
open System

// Problem 46 -- What is the smallest odd composite that cannot be written as the sum of a prime and twice a square

let isPrimeSqrt n =
    let rec check i =
        i > int64(sqrt(double(n))) || (n % i <> 0L && check (i + 1L))
    if n = 1L then false else check 2L   

let gettwiceASquare n =
    lazy(
          let m = int64(n)
          2L * (m * m)
        )

let isPrimeAndTwiceASquare (primes:Set<int64>) (tSquares:Lazy<int64> []) num =
    let rec innerLoop count  =
        printf "%d %d %d %d \n "num count tSquares.[count].Value primes.Count
        match  primes.Contains(num - tSquares.[count].Value), num - tSquares.[count].Value < 0L with
        | true, _ -> true
        | _ , true -> false
        | _ -> //printf "%d %d %d \n" num count tSquares.[count].Value
               innerLoop (count + 1)
               
    innerLoop 0

let findSmallestOddCompositeVal (st:int64) =
    let twiceASquare = [|for i in 1 .. 10000 do yield gettwiceASquare i|]

    let rec getSmallestInternal count (primes:Set<int64>) result =
        match isPrimeSqrt count, result <> 0L with
         | _ , true -> result
         | true, _ -> getSmallestInternal (count + 2L) (primes.Add(count)) result
         | _ -> printf "%d  \n " count 
                let isPrimeAndTS = isPrimeAndTwiceASquare primes twiceASquare count
                getSmallestInternal (count + 2L) primes (if isPrimeAndTS then 0L else count)
    getSmallestInternal st (Set.empty.Add(2L).Add(3L).Add(5L).Add(7L)) 0L


let findSmallestOddCompositeValOld st till start countTill =
    let twiceASquare = [|for i in start .. countTill do yield gettwiceASquare i|]

    let primes = [|st .. till|] |> Array.filter(fun elm -> isPrimeSqrt elm)

    let oddComposite = [|st ..2L .. till |]
                            |> Array.filter(fun elm -> (elm % 3L = 0L || elm % 5L = 0L || elm % 7L = 0L))
    
    let rec findSmallestbadOddComposite (count:int32) (twiceSqrCount:int32) result  =
            match count >= oddComposite.Length ,  result <> 0L with
             | _ ,  true -> result
             | true ,  _ -> 0L                           
             | _ ,  _ ->  let num = oddComposite.[count] - twiceASquare.[twiceSqrCount].Value
                          let isPrime = if num > 0L then Array.tryFind(fun elm -> elm = num) primes <> None else false
                          let newCount, newtsCount = if  isPrime || num < 0L then (count + 1), 0 else count, (twiceSqrCount + 1)
                          let newResult = if num < 0L then oddComposite.[count] else 0L
                          //printf "%d %d %d %d %d %b %d \n"oddComposite.[count] twiceASquare.[twiceSqrCount].Value num newCount newtsCount isPrime newResult
                          findSmallestbadOddComposite newCount newtsCount newResult
    findSmallestbadOddComposite 0 0 0L



// Problem 46 -- What is the smallest odd composite that cannot be written as the sum of a prime and twice a square

