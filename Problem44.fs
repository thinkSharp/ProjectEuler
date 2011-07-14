module Problem44
open System

// Problem 44 -- Find the pair of pentagonal numbers for which their sum and difference is pentagonal with D = pk - pj is minimised

let getPentagonalNum n =
    (n * ((3 * n) - 1) ) / 2

let getPentagonalNums limit =
    let rec generatePen count (result:Set<int>) =
        match count > limit with
         | true -> result
         | _ -> generatePen (count + 1) (result.Add(getPentagonalNum count))
    generatePen 1 Set.empty

let getSmallestDifference limit =
    let pentaNums = getPentagonalNums limit
                        |> Set.toArray
    let len = pentaNums.Length - 1
    let rec getDifferenceInternal i j result =
        match result, i = len, j >= len with 
         | res,isTrue, _ when res <> 0 || isTrue -> result
         | _,_, true -> //printf "%d %d %d \n " i j result 
                        getDifferenceInternal (i + 1) 0 result
         | _, _, _ ->   let pSum = pentaNums.[i] + pentaNums.[j]
                        let pDiff = pentaNums.[j] - pentaNums.[i]
                    
                        let isSumPenta = Array.tryFind(fun elm -> elm = pSum) pentaNums
                        let isDiffPenta = Array.tryFind(fun elm -> elm = pDiff) pentaNums
                        let newResult = if isSumPenta <> None && isDiffPenta <> None then 
                                            let diff = (pentaNums.[j] - pentaNums.[i])
                                            let nDiff = if diff < 0 then diff * -1 else diff
                                            printf "%d %d %d %d %d %d \n " i j pSum pDiff nDiff result
                                            nDiff
                                            
                                        else 
                                            result
                        getDifferenceInternal i (j + 1) newResult
    getDifferenceInternal 0 0 0
    
 // My program take more then 1 minutes so I read some of the thread answers and realized that I do not need to pre calculate the Pentagonal and search from the result
 // I can simply check if the difference is Pentagonal if so then check the sum is also pentagonal, quit once find the first one
    
// Problem 44 -- Find the pair of pentagonal numbers for which their sum and difference is pentagonal with D = pk - pj is minimised


