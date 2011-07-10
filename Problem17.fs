module Problem17
open System

// Problem 17 -- how many letters would be used if we write 1 to 1000 in word
let rec getLength = function
    | x when x = 1 || x = 2 || x = 6  || x = 10 -> 3
    | x when x = 3 || x = 7 || x = 8 -> 5
    | x when x = 4 || x = 5 || x = 9 -> 4
    | x when x = 11 || x = 12  -> 6
    | x when x = 13 || x = 14 || x = 18 || x = 19 -> 8
    | x when x = 15 || x = 16 -> 7
    | x when x = 17 -> 9
    | x when x = 20 || x = 30 || x = 80 || x = 90 -> 6
    | x when x = 40 || x = 50  || x = 60 -> 5
    | x when x = 70 -> 7
    | x when x = 1000 -> 11
    | x when x < 100 -> let rem = x % 10 
                        let co = (x / 10) * 10 
                        getLength co + getLength rem
    | x when x % 100 = 0 -> let co = x / 100
                            getLength co + 7
    | x -> let rem = x % 100
           let co = x / 100 
           getLength co + 10 + getLength rem

let sumLength = {1 .. 1000}
                  |> Seq.map (fun x -> getLength x)
                  |> Seq.sum

 // Problem 17 -- how many letters would be used if we write 1 to 1000 in word
