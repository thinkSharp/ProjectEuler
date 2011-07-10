module Problem7
open System

// Problem 7 10001st Prime finder
let isprime n =

    let rec check i =

        i > n/2L || (n % i <> 0L && check (i + 1L))

    check 2L

 

let rec getNextPrime (i:int64) (isPrm:bool) =

    if  isPrm then

        i

    else

        let x =  isprime (i + 1L)

        getNextPrime (i + 1L) x

 

let getNthPrime n =

   

    let rec getNthPrimeInternal orig count prime =

        if count = orig then

            prime

        else

            let nPrime = getNextPrime prime false

            getNthPrimeInternal orig (count + 1L) nPrime

 

    getNthPrimeInternal n 1L 2L

 

 

// Problem 7 10001st Prime finder
