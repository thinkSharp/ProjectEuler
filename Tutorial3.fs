module Tutorial3
open System

// Problem 23 -- what is the sum of all the positive integers which cannot be written as the sum of two abundant numbers
let getSumOfAllDivisors num =
    let rec getSumOfDivisors count result =
        match count with
         | x when x > (num + 1) /2 -> result
         | _ -> //printf "%d %d \n" count result
                let newResult = if num % count = 0 then result + count else result
                getSumOfDivisors (count + 1) newResult
    getSumOfDivisors 1 0

let getNextAbundantNum num =
    let rec getAbundantNum count result =
        match result > count with
         | true -> count
         | false -> let newCount = count + 1
                    let newResult = getSumOfAllDivisors newCount
                    //printf "%d %d \n" newCount newResult
                    getAbundantNum newCount  newResult
    getAbundantNum num  0

let getAllAbundantNumberLessThen num =
    let rec getAbundantNumber count result =
        //printf "%d \n" count
        match count with
         | x when x > num -> result
         | _ -> let abdNum = getNextAbundantNum count 
                           //let abdNums = getMultiplyOfAbundantNumLessThen abdNum num result
                let newResult = if abdNum < num then  result @ [abdNum]  else result
                //if abdNum % 2 <> 0 then printf"%d \n" abdNum else ()
                           
                getAbundantNumber abdNum newResult 
    let result = getAbundantNumber 11 []
    //List.iter( fun x -> printf "%d \n" x) result
    let res = List.sort result
    List.toArray res

let getSumOfTwoAbndLst (abNums: int []) (limit:int) =
    let res = [| for i in 1 .. limit -> false |]
    let rec getSumOfTwoAbndLst countx county (result: bool []) =
       
        match countx, county with
            | x, y when x >= abNums.Length -> result
            | x, y -> let sum = abNums.[countx] + abNums.[county]
                      //printf "%d - %d - %d \n" x y sum
                      match x,y,sum with
                       |  x,y,sum when y >= abNums.Length -> getSumOfTwoAbndLst ( x + 1 ) 0 result
                       |  x,y,sum when sum >= limit -> getSumOfTwoAbndLst (x + 1) 0 result
                       | _ ->  result.[sum] <- true
                               getSumOfTwoAbndLst x (y + 1) result
    getSumOfTwoAbndLst 0 0 res
         
let sumOfNotSumOfAbnd (abdLst:bool [])= 
    let rec getSumInt count result =
        match count with
         | x when x >= abdLst.Length -> result
         | _ -> let newResult = if abdLst.[count] then
                                   
                                   result 
                                else
                                    //printf "%d %d \n" count result 
                                    result + count
                getSumInt (count + 1) newResult
    getSumInt 0 0
 
let getSum limit =
    let abNums = getAllAbundantNumberLessThen limit
    let x = getSumOfTwoAbndLst abNums limit
    sumOfNotSumOfAbnd x
    //x                  
// Problem 23 -- what is the sum of all the positive integers which cannot be written as the sum of two abundant numbers


