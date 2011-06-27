module Problem26
open System

// Problem 26 -- Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part
let getLength num =
  match num with
   | x when x < 10 -> 100
   | x when x < 100 -> 100
   | _ -> 3000

let buildRemainder divisor  =
    let length = getLength divisor 
    let rec buildRemainder value (result:string) =
        match result with
         | x when x.Length >= length -> result
         | _ -> let co = value / divisor
                let rem = value % divisor
                let newRes = result + co.ToString()
                buildRemainder (rem * 10) newRes
    buildRemainder 10 ""

let getNextFirst pos len counter =
        match pos with
         | x when x = 0 -> (counter + 1) ,1,(counter + 1)
         | _ -> pos - 1, len + 1, counter 

let fst3 (a,_,_) = a
let snd3 (_,b,_) = b
let trd3 (_, _, c) = c

let getRecurringCycle (strNum:string) =
    let ln = strNum.Length 
    let rec findRecurringVal (fstVal:int*int*int) (sndVal:int*int) count reStr =
        let shouldContinue = ( fst sndVal) + (snd sndVal) <= ln && (fst3 fstVal) + (snd3 fstVal) <= ln
        //printf "%d %b %d %d \n" ln shouldContinue ((fst sndVal) + (snd sndVal)) ((fst3 fstVal) + (snd3 fstVal))
        match shouldContinue with
         | false -> count, reStr
         | true -> let fstStr = strNum.Substring(fst3 fstVal, snd3 fstVal)
                   let sndStr = strNum.Substring(fst sndVal, snd sndVal)
                   match fstStr = sndStr with
                    | true->  findRecurringVal fstVal ((fst sndVal) + (snd sndVal) , (snd sndVal)) (count + 1) fstStr
                    | _ ->    let getNewFirst = getNextFirst (fst3 fstVal) (snd3 fstVal) (trd3 fstVal)
                              findRecurringVal getNewFirst ((fst3 getNewFirst) + (snd3 getNewFirst), (snd3 getNewFirst)) 0 fstStr
    
    findRecurringVal (0,1,0) (1,1) 0 ""

let getLongestRecurringCycle limit =
    let rec getLongestRecurringCycleInt num result length =
        match num with
         | x when x >= limit -> result
         | _ -> let rem = buildRemainder num
                let res = getRecurringCycle rem 
                let newRec = snd res
                let newResult = if  newRec.Length > length && (fst res) > 0 then num else result
                let newLength = if newRec.Length > length && (fst res) > 0 then newRec.Length else length
                //printf "%d  %d %d %s %d \n" newResult newLength num newRec (fst res)
                getLongestRecurringCycleInt (num + 2) newResult newLength
    getLongestRecurringCycleInt 3 0 0

        
// Problem 26 -- Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part
