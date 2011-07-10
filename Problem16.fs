module Problem16
open System


// Problem 16 -- the sum of the digit of result of 2^1000

let pow baseX times =
  let rec powInt count result =
     match count with 
      | x when x = times -> result
      | _ -> powInt (count + 1I) (result * baseX)
  powInt 0I 1I

let getSumOfDigitOfPow baseX times =
   let result = pow baseX times
   let sumPow = string(result)
                  
                  |> Seq.fold( fun sum elm -> 
                                  
                                  let intVal = System.Convert.ToInt32(elm.ToString())
                                  printf "%c %d %d \n" elm sum intVal
                                  intVal + sum ) 0
   sumPow


// Problem 16 -- the sum of the digit of result of 2^1000