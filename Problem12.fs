module Problem12
open System

// Problem 12 -- first triangle number to have over five hundred divisors

 

let getNextTriangleNumber tnum = 

    (tnum * (tnum + 1L) ) / 2L

 

let getTriangleNumberDivisorCount tNum =

    let rec getDivisorsCount count  num =

        match num with 

         | x when x = tNum -> count

         | _ -> let newCount = if tNum % num = 0L then count + 1L else count

                getDivisorsCount newCount (num + 1L)

    getDivisorsCount 1L 1L

    

let getTriangleNumberDivisorCountOver tCount =

    let rec getDivisorsCount num pNum result =

        match result with

         | x when x >= tCount -> pNum

         | _ -> let newNum = num + 1L

                let tNum = getNextTriangleNumber newNum

                let result = getTriangleNumberDivisorCount tNum

                if result > 100L then printf "%d %d \n" tNum result else ()

                getDivisorsCount newNum tNum result

    getDivisorsCount 10000L 1L 1L

 

 

// Problem 12 -- first triangle number to have over five hundred divisors