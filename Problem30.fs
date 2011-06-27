module Problem30
open System

// Problem 30 -- Find the sum of all the numbers that can be written as the sum of fifth powers of their digits

let powBigInt (baseA:int) (powB:int) =
  let rec powLoop count result =
    match count = powB with
     | true -> result
     | _ -> powLoop (count + 1) (result * baseA)
  powLoop 0 1

let sumOffifthpowersOfDigit start limit power =
    
    let getSumOfDigits (n:int) =
        let sumDigits = n.ToString().ToCharArray()
                        |> Array.map (fun x -> powBigInt (System.Convert.ToInt32(x.ToString())) power)
                        |> Array.sum 
        sumDigits

    
    let rec getDigitsWithPowerOffifth num result =
        match num = limit with
         | true -> result
         | _ -> let retVal = getSumOfDigits num
                let newRes = if num = retVal then 
                                printf "%d \n" retVal
                                result + retVal 
                             else 
                                result
                getDigitsWithPowerOffifth (num + 1) newRes
    getDigitsWithPowerOffifth start 0




// Problem 30 -- Find the sum of all the numbers that can be written as the sum of fifth pwoers of their digits

