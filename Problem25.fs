module Problem25
open System

// Problem 25 -- What is the first term in the Fibonacci sequence to contain 1000 digits?

let getFibonacciSeq limit =
    let rec getFibonacciSeq first second result term =
        //let newNum = first + second
        printf "%A %A \n" result term
        let ln = result.ToString().Length
        match ln with
         | x when x = limit -> result
         | _ -> getFibonacciSeq second result (second + result) (term + 1I)
    getFibonacciSeq 1I 1I 2I 3I


// Problem 25 -- What is the first term in the Fibonacci sequence to contain 1000 digits?

