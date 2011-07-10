module Problem15
open System


// Problem 15 -- How many routes are there in the 20 X 20 grid

let getFactorial num =
    let rec getFacto n result =
        match n with
         | x when x = 1I -> result
         | _ -> let newN = n - 1I
                let newResult = result * newN
                getFacto  newN newResult
    getFacto (num + 1I) 1I

let getPossibleRouteOfSqGrid num =
   getFactorial (2I * num) / ( getFactorial num * getFactorial num)

let printAllPossibleRoute num =
   let endX = num
   let endY = num
   let rec printAPoint (currentP: int * int) (result:string) =
      match currentP with
       | x,y when x = endX && y = endY ->  printf "%s \n" result
       | x,y when x = endX && y < endY ->  printAPoint (x, y + 1) (result + "D")                                           
       | x,y when x < endX && y = endY ->  printAPoint (x + 1, y) (result + "R")
       | x,y -> printAPoint (x + 1, y) (result + "R")
                printAPoint (x, y + 1) (result + "D")
   printAPoint (0 , 0)  ""

// Problem 15 -- How many routes are there in the 20 X 20 grid