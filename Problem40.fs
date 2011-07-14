module Problem40
open System

// Problem 40 -- find the value of following expression from given fractional path. "d1 * d10 * d100 * d1000 * d10000 * d100000"

let getConcatPositiveIntegerUnder limit =
    let x = {1 .. limit}
            |> Seq.fold (fun state elm -> state + elm.ToString()) ""
    
    x.Length
    //x.Substring(0,1) + " " + x.Substring(9,1) + " " + x.Substring(99,1) + " " + x.Substring(999,1) + " " + x.Substring(9999,1)

let getConcatPositiveIntegerStringBuilder limit =
   let rec innerLoop count result =
        match count = limit with
         | true -> result
         | _ -> innerLoop (count + 1)  (result + count.ToString())
   let x = innerLoop 1 ""
   x.Length

let getConcatPositiveIntegerAsync limit  =
 let x = {1 .. limit }
            |> Seq.map (fun elm -> async { return elm.ToString() })
            |> Async.Parallel
            |> Async.RunSynchronously
            |> Seq.fold(fun sec elm -> sec + elm) ""
 printf "%d \n" x.Length

 //int32(x.Substring(0,1)) //* int32(x.Substring(9,1)) * int32(x.Substring(99,1)) * int32(x.Substring(999,1)) * int32(x.Substring(9999,1)) * int32(x.Substring(99999,1)) * int32(x.Substring(999999,1))
 //int32(x.Substring(0,1)) * int32(x.Substring(9,1)) // * int32(x.Substring(99,1)) * int32(x.Substring(999,1)) * int32(x.Substring(9999,1)) * int32(x.Substring(99999,1)) * int32(x.Substring(999999,1))
 //int32(x.Substring(0,1)) * int32(x.Substring(9,1)) * int32(x.Substring(99,1)) //* int32(x.Substring(999,1)) * int32(x.Substring(9999,1)) * int32(x.Substring(99999,1)) * int32(x.Substring(999999,1)) 
 //int32(x.Substring(0,1)) * int32(x.Substring(9,1)) * int32(x.Substring(99,1)) * int32(x.Substring(999,1)) //* int32(x.Substring(9999,1)) * int32(x.Substring(99999,1)) * int32(x.Substring(999999,1))
 //int32(x.Substring(0,1)) * int32(x.Substring(9,1)) * int32(x.Substring(99,1)) * int32(x.Substring(999,1)) * int32(x.Substring(9999,1)) //* int32(x.Substring(99999,1)) * int32(x.Substring(999999,1))
 //int32(x.Substring(0,1)) * int32(x.Substring(9,1)) * int32(x.Substring(99,1)) * int32(x.Substring(999,1)) * int32(x.Substring(9999,1)) * int32(x.Substring(99999,1)) //* int32(x.Substring(999999,1))
 int32(x.Substring(0,1)) * int32(x.Substring(9,1)) * int32(x.Substring(99,1)) * int32(x.Substring(999,1)) * int32(x.Substring(9999,1)) * int32(x.Substring(99999,1)) * int32(x.Substring(999999,1))
// Problem 40 -- find the value of following expression from given fractional path. "d1 * d10 * d100 * d1000 * d10000 * d100000"

