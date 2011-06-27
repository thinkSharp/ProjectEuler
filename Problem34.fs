module Problem34
open System

//Problem 34 Find the sum of all numbers which are equal to the sum of the factorial of their digits
let factorial x =
    let rec factorial count result =
        match count > x with
         | true ->  //printf "%d %d \n" x result
                    result
         | _ -> factorial (count + 1) (result * count)
    factorial 1 1

let isSumOfFacOfDigSameAsNum num =
    let x = num.ToString().ToCharArray()
            |> Array.map( fun elm -> 
                                     System.Convert.ToInt32(elm.ToString()))
            |> Array.map( fun elm -> factorial elm)
            |> Array.sum
    if num = int64(x) then printf "%d %d \n" num x        
    num = int64(x)

let getSumOfAllNumUnder limit =
    let x = [|3L .. limit |]
            |> Array.filter(fun elm -> isSumOfFacOfDigSameAsNum elm)
            |> Array.sum
    x

//Problem 34 Find the sum of all numbers which are equal to the sum of the factorial of their digits


