module Problem24
open System

// Problem 24 What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?

let twoDigitPerm org x y res nth =
    let rec twoDigitPermInt x y count result =
        match count,result with
         | z,r when r = nth -> printf "the last is the result"
                               result, "done"
         | z,r when z = 2 -> result, ""
         | _ -> printf "%s %s %s %d \n" org x y result
                twoDigitPermInt y x (count + 1) (result + 1)
    twoDigitPermInt x y 0 res

let threeDigitPerm org x y z res nth =
    let retVal = twoDigitPerm (org + " " + x) y z res nth
    match retVal with
     | x , y when y = "done" -> x , y
     | _ -> let retVal = twoDigitPerm (org + " " + y) x z (fst retVal) nth
            match retVal with
               | x, y when y = "done" -> x, y
               | _ -> twoDigitPerm (org + " " + z) x y (fst retVal) nth

let fourDigitPerm org w x y z res nth =
    let retVal = threeDigitPerm (org + " " + w) x y z res nth 
    match retVal with
     | x, y when y = "done" -> x , y
     | _ -> let retVal = threeDigitPerm (org + " " + x) w y z (fst retVal) nth
            match retVal with
             | x, y when y = "done" -> x, y
             | _ -> let retVal = threeDigitPerm (org + " " + y) w x z (fst retVal) nth
                    match retVal with
                     | x, y when y = "done" -> x, y
                     | _ -> threeDigitPerm (org + " " + z) w x y (fst retVal) nth

let fiveDigitPerm org v w x y z res nth =
    let retVal = fourDigitPerm (org + " " + v) w x y z res nth
    match retVal with
     | x, y when y = "done" -> x, y
     | _ -> let retVal = fourDigitPerm (org + " " + w) v x y z (fst retVal) nth
            match retVal with
            | x, y when y = "done" -> x, y
            | _ -> let retVal = fourDigitPerm (org + " " + x) w v y z (fst retVal) nth
                   match retVal with
                    | x, y when y = "done" -> x, y
                    | _ -> let retVal = fourDigitPerm (org + " " + y) w v x  z (fst retVal) nth
                           match retVal with
                            | x, y when y = "done" -> x, y
                            | _ -> fourDigitPerm (org + " " + z) w v x y (fst retVal) nth

let sixDigitPerm org u v w x y z res nth =
    let retVal = fiveDigitPerm (org + " " + u) v w x y z res nth
    match retVal with
     | x, y when y = "done" -> x, y
     | _ -> let retVal = fiveDigitPerm (org + " " + v) u w x y z (fst retVal) nth
            match retVal with
            | x, y when y = "done" -> x, y
            | _ -> let retVal = fiveDigitPerm (org + " " + w) v u x y z (fst retVal) nth
                   match retVal with
                    | x, y when y = "done" -> x, y
                    | _ -> let retVal = fiveDigitPerm (org + " " + x) u v w y z (fst retVal) nth
                           match retVal with
                            | x, y when y = "done" -> x, y
                            | _ -> let retVal = fiveDigitPerm (org + " " + y) u v w x z (fst retVal) nth
                                   match retVal with
                                    | x, y when y = "done" -> x, y
                                    | _ -> fiveDigitPerm (org + " " + z) u v w x y (fst retVal) nth
let sevenDigitPerm org t u v w x y z res nth =
    let retVal = sixDigitPerm (org + " " + t) u v w x y z res nth
    match retVal with
       | a, b when b = "done" -> a, b
       | _ -> let retVal = sixDigitPerm (org + " " + u) t v w x y z (fst retVal) nth
              match retVal with
                | a, b when b = "done" -> a, b
                | _ -> let retVal = sixDigitPerm (org + " " + v) t u w x y z (fst retVal) nth
                       match retVal with
                        | a, b when b = "done" -> a, b
                        | _ -> let retVal = sixDigitPerm (org + " " + w) t u v x y z (fst retVal) nth
                               match retVal with
                                | a, b when b = "done" -> a, b
                                | _ -> let retVal = sixDigitPerm (org + " " + x) t u v w y z (fst retVal) nth
                                       match retVal with
                                        | a, b when b = "done" -> a, b
                                        | _ -> let retVal = sixDigitPerm (org + " " + y) t u v w x z (fst retVal) nth
                                               match retVal with
                                                | a, b when b = "done" -> a, b
                                                | _ -> sixDigitPerm (org + " " + z) t u v w x y (fst retVal) nth
let eightDigitPerm org s t u v w x y z res nth =
    let retVal = sevenDigitPerm (org + " " + s) t u v w x y z res nth
    match retVal with
     | a, b when b = "done" -> a, b
     | a, b -> let retVal = sevenDigitPerm (org + " " + t) s u v w x y z a nth
               match retVal with
                | a, b when b = "done" -> a, b
                | a, b -> let retVal = sevenDigitPerm (org + " " + u) s t v w x y z a nth
                          match retVal with
                            | a, b when b = "done" -> a, b
                            | a, b -> let retVal = sevenDigitPerm (org + " " + v) s t u w x y z a nth
                                      match retVal with
                                        | a, b when b = "done" -> a, b
                                        | a, b -> let retVal = sevenDigitPerm (org + " " + w) s t u v x y z a nth
                                                  match retVal with
                                                    | a, b when b = "done" -> a, b
                                                    | a, b -> let retVal = sevenDigitPerm (org + " " + x) s t u v w  y z a nth
                                                              match retVal with
                                                                | a, b when b = "done" -> a, b
                                                                | a, b -> let retVal = sevenDigitPerm (org + " " + y) s t u v w x z a nth
                                                                          match retVal with
                                                                            | a, b when b = "done" -> a, b
                                                                            | a, b -> sevenDigitPerm (org + " " + z) s t u v w x y a nth
   
let nineDigitPerm org r s t u v w x y z res nth =
    let retVal = eightDigitPerm (org + " " + r) s t u v w x y z res nth
    match retVal with
     | a, b when b = "done" -> a, b
     | a, b -> let retVal = eightDigitPerm (org + " " + s) r t u v w x y z a nth
               match retVal with
                | a, b when b = "done" -> a, b
                | a, b -> let retVal = eightDigitPerm (org + " " + t) r s u v w x y z a nth
                          match retVal with
                            | a, b when b = "done" -> a, b
                            | a, b -> let retVal = eightDigitPerm (org + " " + u) r s t v w x y z a nth
                                      match retVal with
                                        | a, b when b = "done" -> a, b
                                        | a, b -> let retVal = eightDigitPerm (org + " " + v) r s t u w x y z a nth
                                                  match retVal with
                                                    | a, b when b = "done" -> a, b
                                                    | a, b -> let retVal = eightDigitPerm (org + " " + w) r s t u v x y z a nth
                                                              match retVal with
                                                                | a, b when b = "done" -> a, b
                                                                | a, b -> let retVal = eightDigitPerm (org + " " + x) r s t u v w y z a nth
                                                                          match retVal with
                                                                            | a, b when b = "done" -> a, b
                                                                            | a, b -> let retVal = eightDigitPerm (org + " " + y) r s t u v w x z a nth
                                                                                      match retVal with
                                                                                        | a, b when b = "done" -> a, b
                                                                                        | a, b -> eightDigitPerm (org + " " + z) r s t u v w x y a nth

let tenDigitPerm org q r s t u v w x y z res nth =
    let retVal = nineDigitPerm (org + " " + q) r s t u v w x y z res nth
    match retVal with
     | a, b when b = "done" -> a, b
     | a, b -> let retVal = nineDigitPerm (org + " " + r) q s t u v w x y z a nth
               match retVal with
                | a, b when b = "done" -> a, b
                | a, b -> let retVal = nineDigitPerm (org + " " + s) q r t u v w x y z a nth
                          match retVal with
                            | a, b when b = "done" -> a, b
                            | a, b -> let retVal = nineDigitPerm (org + " " + t) q r s u v w x y z a nth
                                      match retVal with
                                        | a, b when b = "done" -> a, b
                                        | a, b -> let retVal = nineDigitPerm (org + " " + u) q r s t v w x y z a nth
                                                  match retVal with
                                                    | a, b when b = "done" -> a, b
                                                    | a, b -> let retVal = nineDigitPerm (org + " " + v) q r s t u w x y z a nth
                                                              match retVal with
                                                                | a, b when b = "done" -> a, b
                                                                | a, b -> let retVal = nineDigitPerm (org + " " + w) q r s t u v x y z a nth
                                                                          match retVal with
                                                                            | a, b when b = "done" -> a, b
                                                                            | a, b -> let retVal = nineDigitPerm (org + " " + x) q r s t u v w y z a nth
                                                                                      match retVal with
                                                                                        | a, b when b = "done" -> a, b
                                                                                        | a, b -> let retVal = nineDigitPerm (org + " " + y) q r s t u v w x z a nth
                                                                                                  match retVal with
                                                                                                    | a, b when b = "done" -> a, b
                                                                                                    | a, b -> nineDigitPerm (org + " " + z) q r s t u v w x y a nth

// Problem 24 What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?

