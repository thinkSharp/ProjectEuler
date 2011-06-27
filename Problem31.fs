module Problem31
open System

// Problem 31 How many different ways can 2 be made using any number of coins?

let rec onePence remCoins coins localCount =
    match remCoins + (1 * localCount) = 200 with
     | true ->  1
     | _ -> onePence remCoins coins (localCount + 1)

let rec twoPence coins localCount result =
     match localCount = -1 with
     | true -> result
     | _ -> let remCoin  = coins - ( 2 * localCount) 
            //printf "Two: %d %d %d \n" remCoin result localCount
            twoPence coins (localCount - 1) (result + 1)

let rec fivePence coins localCount result =
    match localCount = -1 with
     | true -> result
     | _ -> let remCoin = coins - (5 * localCount)
            let newRes =  (result + (if remCoin = 0 then 1 else twoPence remCoin (remCoin / 2) 0))
            //printf "Five: %d %d %d \n" remCoin newRes localCount
            fivePence coins (localCount - 1) newRes
                
let rec tenPence coins localCount result =
    match localCount = -1 with
     | true -> result
     | _ -> let remCoin = coins - (10 * localCount)
            tenPence coins (localCount - 1) (result + (if remCoin = 0 then 1 else fivePence remCoin (remCoin / 5) 0))

let rec twentyPence coins localCount result =
    match localCount = -1 with
     | true -> result
     | _ -> let remCoin = coins - (20 * localCount)
            twentyPence coins (localCount - 1) (result + (if remCoin = 0 then 1 else tenPence remCoin (remCoin / 10) 0))

let rec fiftyPence coins localCount result =
    match localCount = -1 with
     | true -> result
     | _ -> let remCoin = coins - (50 * localCount)
            fiftyPence coins (localCount - 1) (result + (if remCoin = 0 then 1 else twentyPence remCoin (remCoin / 20) 0))
    
let rec hundredPence coins localCount result =
    match localCount = -1 with
     | true -> result
     | _ -> let remCoin = coins - (100 * localCount)
            hundredPence coins (localCount - 1) (result + (if remCoin = 0 then 1 else fiftyPence remCoin (remCoin / 50) 0))       

let rec twoHundredPence coins localCount result =
    match localCount = -1 with
     | true -> result
     | _ -> let remCoin = coins - (200 * localCount)
            twoHundredPence coins (localCount - 1) (result + (if remCoin = 0 then 1 else hundredPence remCoin (remCoin / 100) 0))

// second Approach
let getCount =
    seq {for i in 200 .. -200 .. 0 do 
                  for j in i .. -100 .. 0 do
                      for k in j .. -50 .. 0 do
                        for l in k .. -20 .. 0 do
                          for m in l .. -10 .. 0 do
                            for n in m .. -5 .. 0 do
                              for o in n .. -2 .. 0 do
                                   yield o
                  } |> Seq.length 
    
        
// Problem 31 How many different ways can 2 be made using any number of coins?