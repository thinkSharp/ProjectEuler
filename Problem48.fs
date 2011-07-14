module Problem48
open System

// problem 48 -- Find the last ten digits of the series from 1 to 1000

let someOfSquare (digit) =
   let rec innerLoop (num) (count:int32) (result:System.Numerics.BigInteger) =
    match count = num with
     | true -> result
     | _ -> innerLoop num (count + 1) (result * System.Numerics.BigInteger(num))
   
   [|1 .. digit|] |> Array.map(fun elm -> innerLoop elm 0  1I)
                   |> Array.sum
                            

// problem 48 -- Find the last ten digits of the series from 1 to 1000

