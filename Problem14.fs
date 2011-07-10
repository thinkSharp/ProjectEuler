module Problem14
open System


// Problem 14 -- Produce Longest chain from starting number less then one million using following fomula n -> n/2 for positive n -> 3n + 1 for negative
let getLargestSeqCountUnder value =
   let rec getSeq num count =
    match num with
     | x when x = 1L -> count
     | _ -> let newCount = count + 1L
            let nextNum = if num % 2L = 0L then num /2L else (3L * num) + 1L
            //printf " From GetSeq %d %d \n" newCount nextNum
            getSeq nextNum newCount
   let rec getSeqcountInter num maxCount maxTNum =
    match num with
     | x when x >= value -> maxTNum 
     | _ -> let newNum = num + 1L

            let count = getSeq newNum 1L
            let newCount = if count > maxCount then count else maxCount
            let newTNum = if newCount <> maxCount then newNum else maxTNum

            //printf " From GetMaxCount %d %d \n" newCount newNum
            getSeqcountInter newNum newCount newTNum

   getSeqcountInter 500000L 1L 1L

// Problem 14 -- Produce Longest chain from starting number less then one million

