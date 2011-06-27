module Problem32
open System

// Problem 32 -- Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital
let isPandigital (str:string) =
//    let sm = str.ToCharArray()
//                 |> Array.map(fun elm -> System.Convert.ToInt32(elm.ToString()))
//                 |> Array.sum

    str.Length = 9 && str.Contains("1") && str.Contains("2") && str.Contains("3") && str.Contains("4") && str.Contains("5") && str.Contains("6") && str.Contains("7") && str.Contains("8") && str.Contains("9")

let rec getSumOfPandigitalProduct i j prd limit (values:Set<int64>):int64 =
    match i = limit, j, prd.ToString().Length > 10 with
     | true,_,_ -> values |> Set.toArray |> Array.sum
     | _,x,t when x = (limit + 1) || t ->getSumOfPandigitalProduct (i + 1) 1 "" limit values
     | _ -> let prd = int64(i * j)
            let valToStr = i.ToString() + j.ToString() + prd.ToString()
            let isP = isPandigital valToStr
            //printf " %d %d %d \n" i j prd
            let newValue = if  isP then values.Add prd else values
            getSumOfPandigitalProduct (i) (j + 1) (valToStr.ToString()) limit newValue


// Problem 32 -- Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital

