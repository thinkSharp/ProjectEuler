module Problem27
open System

// Problem 27 Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum number of primes for consecutive values of n, starting with n = 0.

let isprimeSqrt n =
    let rec check i =
        i > int(sqrt(double(n))) || (n % i <> 0 && check (i + 1))
    check 2    

let fst3 ( a, _ ,_ ) = a
let snd3 (_, b, _ ) = b
let trd3 (_, _, c) = c

let CheckQuadraticValue =
   let getQuadraticValue a b n =
         n * n + (a * n) + b

   let rec getMaxPrimeSeries a b (n:int) count (result: int * int * int) =
        match a, b with
         | x, y when x >=1000 && y >= 1000 -> result
         | _ ->
                let num = getQuadraticValue a b n
                let isPrime = if num < 0 then false else isprimeSqrt num
                match isPrime with
                 | true -> getMaxPrimeSeries a b (n + 1) (count + 1) result
                 | _ -> let newResult = if (fst3 result) < count then (count, a, b) else result
                        let nextb = if b >= 1000 then -999 else (b + 1)
                        let nexta = if b >= 1000 then (a + 1) else a
                        printf "%d %d %d %d %d \n" nexta nextb (fst3 newResult)  (snd3 newResult) (trd3 newResult)
                        getMaxPrimeSeries nexta nextb 0 0 newResult
   let res = getMaxPrimeSeries -999 -999 0 0 (0,0,0)
   (snd3 res) * (trd3 res)

let getMaxPrimeSeriesTest s1 s2 v1 v2 =
    let getQuadraticValue a b n =
         n * n + (a * n) + b

    let rec getMaxPrimeSeriesTest a b (n:int) count (result: int * int * int) =
        match a, b with
         | x, y when x >=v1 && y >= v2 -> result
         | _ ->
                let num = getQuadraticValue a b n
                let isPrime = if num < 0 then false else  isprimeSqrt num
                match isPrime with
                 | true -> getMaxPrimeSeriesTest a b (n + 1) (count + 1) result
                 | _ -> let newResult = if (fst3 result) < count then (count, a, b) else result
                        let nextb = if b >= v2 then s2 else (b + 1)
                        let nexta = if b >= v2 then (a + 1) else a
                        printf "%d %d %d %d %d \n" nexta nextb (fst3 newResult)  (snd3 newResult) (trd3 newResult)
                        getMaxPrimeSeriesTest nexta nextb 0 0 newResult
    getMaxPrimeSeriesTest s1 s2 0 0 (0,0,0)


// Problem 27 Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum number of primes for consecutive values of n, starting with n = 0.

