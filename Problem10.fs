module Problem10
open System

// Problem 10 -- Sum all prime below 2000000

 

let isprimeSqrt n =

    let rec check i =

        i > int64(sqrt(double(n))) || (n % i <> 0L && check (i + 1L))

    check 2L    

 

let rec getNextPrimeSqrt (i:int64) (isPrm:bool) =

    if  isPrm then

        i

    else

        let x =  isprimeSqrt (i + 1L)

        getNextPrimeSqrt (i + 1L) x

 

let getSumOfPrimeBelow value =

  let rec SumOfPrimes lastPrime sum =

       match lastPrime with

        | x when x > value -> sum

        | _ -> let newSum = sum + lastPrime

               let nextPrime = getNextPrimeSqrt lastPrime false

               //printf "%d %d \n" newSum nextPrime

               SumOfPrimes nextPrime newSum

  SumOfPrimes 2L 0L

 

// Problem 10 -- Sum all prime below 2000000