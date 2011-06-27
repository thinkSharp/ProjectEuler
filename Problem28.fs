module Problem28
open System

// Problem 28 -- What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed

let getTheSumOfDiagonalsOfSpiralBy num =
    let prod = num * num
    let rec sumOfDiagonals currentNum skip result =
        match currentNum >= prod with
         | true -> result
         | _ -> let newSkip = skip + 2
                let num1 = (currentNum + newSkip + 1)
                let num2 = (num1 + newSkip + 1)
                let num3 = (num2 + newSkip + 1)
                let num4 = (num3 + newSkip + 1)
                sumOfDiagonals num4 newSkip (result + num1 + num2 + num3 + num4)
    let result = 1 + 3 + 5 + 7 + 9
    sumOfDiagonals 9 1 result
       

// Problem 28 -- What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed

