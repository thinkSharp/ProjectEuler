module Problem38
open System

// Problem 38 -- What is the largest pandigital 9-digit number that can be formed as the concatenated product of an integer
let isPandigital (str:string) =
    str.Length = 9 && str.Contains("1") && str.Contains("2") && str.Contains("3") && 
    str.Contains("4") && str.Contains("5") && str.Contains("6") && str.Contains("7") && 
    str.Contains("8") && str.Contains("9")

let getLargestPandigitalNum limit =
    let rec innerLoop left right (interValue:string) (largestPanNum:int64) =
        match left > limit , interValue.Length  with
         | true,_ -> largestPanNum
         | _ , y when y < 9 -> innerLoop left (right + 1) (interValue + (left * right).ToString()) largestPanNum
         | _ , y when y = 9 -> let value = int64(interValue)
                               //printf "%d %d %d %s %d \n" left right value interValue largestPanNum
                               innerLoop (left + 1) 1 "" (if isPandigital(interValue) && largestPanNum < value then value else largestPanNum)
         | _ , _  -> innerLoop (left + 1) 1 "" largestPanNum
    innerLoop 2 1 "" 0L


// Problem 38 -- What is the largest pandigital 9-digit number that can be formed as the concatenated product of an integer

