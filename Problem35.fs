module Problem35
open System

// Problem 35 How many circular primes are there below one million?
let isprimeSqrt n =
    let rec check i =
        i > int(sqrt(double(n))) || (n % i <> 0 && check (i + 1))
    check 2   

let isCircularPrime num =
    


// Problem 35 How many circular primes are there below one million?

